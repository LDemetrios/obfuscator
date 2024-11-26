
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import org.ldemetrios.*
import org.ldemetrios.JavaParser.MethodDeclarationContext
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths


val ParseTree.children get() = (0 until childCount).asSequence().map(this::getChild)

fun main() {
    // Load input file
    val input = File("/home/ldemetrios/Workspace/ITMO/MT/Obfuscator/src/main/resources/Test").readText()

    val t = System.currentTimeMillis()
    println("Start")
    // Use generated Lexer and Parser
    val lexer = JavaLexer(CharStreams.fromString(input))
    val tokens: CommonTokenStream = CommonTokenStream(lexer)
    val parser = JavaParser(tokens)

    // Parse and print tree
    val tree = parser.file() // Assuming "prog" is the root rule
    println("End ${System.currentTimeMillis() - t}")

//    tree
//        .classDeclaration().first()
//        .children.filterIsInstance<MethodDeclarationContext>()
//        .forEach{
////            println(it.toStringTree(parser))
//            println(it.methodHeader().methodDeclarator().identifier().toStringTree(parser))
//        }
}