plugins {
    kotlin("jvm") version "2.0.20"
    antlr
    id("antlr")
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    implementation("org.antlr:antlr4-runtime:4.+") // Replace "x" with the latest version
    antlr("org.antlr:antlr4:4.+")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}

//tasks.generateGrammarSource {
//    this.outputDirectory = File("src/main/antlr-gen")
////    maxHeapSize = "64m"
////    arguments = arguments + listOf("-visitor", "-long-messages")
//}

tasks.compileKotlin {
    dependsOn("generateGrammarSource")
}

tasks.generateGrammarSource {
    arguments = arguments + listOf("-package", "org.ldemetrios")
    outputDirectory = File("$buildDir/generated-src/antlr/main/org/ldemetrios")
}