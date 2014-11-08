/*
 * Copyright 2011 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.twitter.scrooge.backend

import com.twitter.scrooge.ast._
import com.twitter.scrooge.frontend.{ScroogeInternalException, ResolvedDocument}
import com.twitter.scrooge.mustache.Dictionary._
import java.io.File
import com.twitter.scrooge.mustache.HandlebarLoader
import org.apache.commons.lang.StringEscapeUtils.escapeHtml

object HtmlGeneratorFactory extends GeneratorFactory {
  val lang = "html"
  val handlebarLoader = new HandlebarLoader("/htmlgen/", ".html")
  def apply(
             includeMap: Map[String, ResolvedDocument],
             defaultNamespace: String,
             experimentFlags: Seq[String]
             ): ThriftGenerator = new HtmlGenerator(includeMap, defaultNamespace, experimentFlags, handlebarLoader)
}

class HtmlGenerator(
                     val includeMap: Map[String, ResolvedDocument],
                     val defaultNamespace: String,
                     val experimentFlags: Seq[String],
                     val templatesLoader: HandlebarLoader
                     ) extends TemplateGenerator{
  def templates: HandlebarLoader = templatesLoader

  val fileExtension = ".html"
  val templateDirName = "/htmlgen/"

  var warnOnJavaNamespaceFallback: Boolean = false

  //TODO Need to revise this method for HTML
  // Quote HTML reserved words in ``
  def quoteKeyword(str: String): String =
       escapeHtml(str)

  def processComment(comment: String): String = {
      comment.replaceAll("/\\*\\*|\\*/|\\s\\*\\s", " ")
        .replaceFirst("@param\\s([a-zA-Z]*)\\s","<br><b>Parameters</b><br> - <i>$1<\\/i> ")
        .replaceAll("@param\\s([a-zA-Z]*)\\s","<br> - <i>$1<\\/i> ")
        .replaceAll("@code\\s([a-zA-Z]*)\\s", "<code>$1</code>")
        .replaceAll("@return\\s(.*)\\n", "<p> <b> Returns: <\\/b> <i>$1<\\/i><br>")
        .replaceAll("\\{\\s*@link\\s*[[a-z][\\.]]*([a-zA-Z]*)\\s*[a-zA-Z]*\\}", "<b><i>$1</i></b>")
  }


  def normalizeCase[N <: Node](node: N) = {
    (node match {
      case d: Document =>
        d.copy(defs = d.defs.map(normalizeCase(_)))
      case id: Identifier => id.toTitleCase
      case e: EnumRHS =>
        e.copy(normalizeCase(e.enum), normalizeCase(e.value))
      case f: Field =>
        f.copy(
          sid = f.sid.toCamelCase,
          comment = f.comment match { case Some(x) => Some(processComment(x)) case None=> Some("")},
          default = f.default.map(normalizeCase(_)))
      case f: Function =>
        f.copy(
          args = f.args.map(normalizeCase(_)),
          docstring = f.docstring match { case Some(x) => Some(processComment(x)) case None=> Some("")},
          throws = f.throws.map(normalizeCase(_)))
      case c: ConstDefinition =>
        c.copy(value = normalizeCase(c.value))
      case e: Enum =>
        e.copy(values = e.values.map(normalizeCase(_)),
               docstring = e.docstring match { case Some(x) => Some(processComment(x)) case None=> Some("")})
      case e: EnumField =>
        e.copy(sid = e.sid.toTitleCase,
               docstring = e.docstring match { case Some(x) => Some(processComment(x)) case None=> Some("")})
      case s: Struct =>
        s.copy(fields = s.fields.map(normalizeCase(_)),
          docstring = s.docstring match { case Some(x) => Some(processComment(x)) case None=> Some("")}
          )
      case f: FunctionArgs =>
        f.copy(fields = f.fields.map(normalizeCase(_)))
      case f: FunctionResult =>
        f.copy(fields = f.fields.map(normalizeCase(_)))
      case e: Exception_ =>
        e.copy(fields = e.fields.map(normalizeCase(_)))
      case s: Service =>
        s.copy(functions = s.functions.map(normalizeCase(_)))
      case n => n
    }).asInstanceOf[N]
  }

  private[this] def getNamespaceWithWarning(doc: Document): Option[Identifier] =
    doc.namespace("scala") orElse {
      val ns = doc.namespace("java")
      if (ns.isDefined && warnOnJavaNamespaceFallback)
        println("falling back to the java namespace. this will soon be deprecated")
      ns
    }
  // methods that convert AST nodes to CodeFragment
  override def genID(data: Identifier): CodeFragment = data match {
    case SimpleID(name, _) => codify(quoteKeyword(name) )
    case QualifiedID(names) => codify(names.map { quoteKeyword(_) }.mkString("/"))
  }

  override def getNamespace(doc: Document): Identifier =
    getNamespaceWithWarning(doc) getOrElse (SimpleID(defaultNamespace))

  def genList(list: ListRHS, mutable: Boolean = false): CodeFragment = {
    val code = "["+
      list.elems.map(genConstant(_).toData).mkString(", ") + "]"
    codify(code)
  }

  def genSet(set: SetRHS, mutable: Boolean = false): CodeFragment = {
    val code = "["+
      set.elems.map(genConstant(_).toData).mkString(", ") + "]"
    codify(code)
  }

  def genMap(map: MapRHS, mutable: Boolean = false): CodeFragment = {
    val code = "{"+ (map.elems.map {
      case (k, v) =>
        genConstant(k).toData + " -> " + genConstant(v).toData
    } mkString (", ")) + "}"
    codify(code)
  }

  def genEnum(enum: EnumRHS, fieldType: Option[FieldType] = None): CodeFragment = {
    def getTypeId: Identifier = fieldType.getOrElse(Void) match {
      case n: NamedType => qualifyNamedType(n)
      case _ => enum.enum.sid
    }
    genID(enum.value.sid.toTitleCase.addScope(getTypeId.toTitleCase))
  }

  // TODO
  def genStruct(struct: StructRHS): CodeFragment =
    throw new Exception("not implemented")

  override def genDefaultValue(fieldType: FieldType, mutable: Boolean = false): CodeFragment = {
    val code = fieldType match {
      case TI64 => "0L"
      case MapType(_, _, _) | SetType(_, _) | ListType(_, _) =>
        genType(fieldType, None ,mutable).toData + "()"
      case _ => super.genDefaultValue(fieldType, mutable).toData
    }
    codify(code)
  }

  /**
   * Generates a suffix to append to a field expression that will
   * convert the value to an immutable equivalent.
   */
  def genToImmutable(t: FieldType): CodeFragment = {
    val code = t match {
      case MapType(_, _, _) => ".toMap"
      case SetType(_, _) => ".toSet"
      case ListType(_, _) => ".toList"
      case _ => ""
    }
    codify(code)
  }

  /**
   * Generates a suffix to append to a field expression that will
   * convert the value to an immutable equivalent.
   */
  def genToImmutable(f: Field): CodeFragment = {
    if (f.requiredness.isOptional) {
      val code = genToImmutable(f.fieldType).toData match {
        case "" => ""
        case underlyingToImmutable => ".map(_" + underlyingToImmutable + ")"
      }
      codify(code)
    } else {
      genToImmutable(f.fieldType)
    }
  }

  /**
   * Generates a prefix and suffix to wrap around a field expression that will
   * convert the value to a mutable equivalent.
   */
  def toMutable(t: FieldType): (String, String) = {
    t match {
      case MapType(_, _, _) | SetType(_, _) => (genType(t, None, true).toData + "() ++= ", "")
      case ListType(_, _) => ("", ".toBuffer")
      case _ => ("", "")
    }
  }

  /**
   * Generates a prefix and suffix to wrap around a field expression that will
   * convert the value to a mutable equivalent.
   */
  def toMutable(f: Field): (String, String) = {
    if (f.requiredness.isOptional) {
      toMutable(f.fieldType) match {
        case ("", "") => ("", "")
        case (prefix, suffix) => ("", ".map(" + prefix + "_" + suffix + ")")
      }
    } else {
      toMutable(f.fieldType)
    }
  }

  override def processFileName(sid: SimpleID, namespace: Option[Identifier]):String =
   namespace match {
      case Some(n) => genID(n).toData.replaceAll("/","_")+"_"+sid.toTitleCase.name
      case None => sid.toTitleCase.name
  }

  def genLinksForNamedTypes(namespace: Option[Identifier], t: FunctionType):String = {
    val workingDir = new File(".").getCanonicalPath
    val linkDisplay = namespace match {
      case Some(n) => genID(n).toData
      case None => ""
    }

    val link = t match {
      case n: NamedType => if(isPrimitive(t)){
        genID(qualifyNamedType(n).toTitleCase).toData
      }else{
        val struct: CodeFragment = genID(qualifyNamedType(n, true).toTitleCase)
        "<a href=\"" + (if( !n.scopePrefix.isDefined ) linkDisplay.replaceAll("/","_") + "_" else "" )+
          struct.toData.replaceAll("/","_")+".html\">"+
          genID(qualifyNamedType(n).toTitleCase)+"</a>"
      }
      case _ => ""
    }
    link
  }

  //TODO Consider adding link generation here. Perhaps around the named type case?
  def genType(t: FunctionType, namespace: Option[Identifier], mutable: Boolean ): CodeFragment = {
    val code = t match {
      case Void => "Void"
      case OnewayVoid => "Void"
      case TBool => "Boolean"
      case TByte => "Byte"
      case TI16 => "Short"
      case TI32 => "Int"
      case TI64 => "Long"
      case TDouble => "Double"
      case TString => "String"
      case TBinary => "ByteBuffer"
      case MapType(k, v, _) =>
        "Map[" + genType(k, namespace, mutable).toData + ", " + genType(v, namespace, mutable).toData+ "]"
      case SetType(x, _) =>
       "Set[" + genType(x, namespace, mutable).toData + "]"
      case ListType(x, _) =>
       "List[" + genType(x, namespace, mutable).toData + "]"
      case n: NamedType => genLinksForNamedTypes(namespace, t)
      case r: ReferenceType =>
        throw new ScroogeInternalException("ReferenceType should not appear in backend")
    }
    codify(code)
  }

  def genPrimitiveType(t: FunctionType, mutable: Boolean = false): CodeFragment = genType(t, None,mutable)

  def genFieldType(f: Field, namespace: Option[Identifier], mutable: Boolean = false): CodeFragment = {
    val baseType = genType(f.fieldType, namespace, mutable).toData
    codify(baseType)
  }

  def genFieldParams(fields: Seq[Field], namespace: Option[Identifier], asVal: Boolean = false): CodeFragment = {
    val code = fields.map {
      f =>
        val valPrefix = if (asVal) "val " else ""
        val nameAndType = genFieldType(f, namespace).toData + " " + genID(f.sid).toData
        valPrefix + nameAndType
    }.mkString(", ")
    codify(code)
  }

  def genBaseFinagleService: CodeFragment = codify("FinagleService[Array[Byte], Array[Byte]]")

  def getParentFinagleService(p: ServiceParent): CodeFragment =
    genID(Identifier(getServiceParentID(p).fullName + "$FinagleService"))

  def getParentFinagleClient(p: ServiceParent): CodeFragment =
    genID(Identifier(getServiceParentID(p).fullName + "$FinagleClient"))

  override def finagleClientFile(
                                  packageDir: File,
                                  service: Service, options:
  Set[ServiceOption]
                                  ): Option[File] =
    options.find(_ == WithFinagle) map {
      _ =>
        new File(packageDir, service.sid.toTitleCase.name + "$FinagleClient" + fileExtension)
    }

  override def finagleServiceFile(
                                   packageDir: File,
                                   service: Service, options:
  Set[ServiceOption]
                                   ): Option[File] =
    options.find(_ == WithFinagle) map {
      _ =>
        new File(packageDir, service.sid.toTitleCase.name + "$FinagleService" + fileExtension)
    }


}
