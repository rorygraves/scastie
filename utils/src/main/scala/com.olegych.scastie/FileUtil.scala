package com.olegych.scastie

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import System.{lineSeparator => nl}
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{PosixFileAttributeView, PosixFilePermission}
import java.util

object FileUtil {
  def makeExecutable(file: Path) = {
    val attributeView = Files.getFileAttributeView(file, classOf[PosixFileAttributeView])
    attributeView.setPermissions( util.EnumSet.of(PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.OWNER_READ,PosixFilePermission.OWNER_WRITE))
  }

  def createTempDir(prefix: String)(implicit userAndGroup: UserAndGroup): Path = {
    val path = Files.createTempDirectory(prefix)
    println("PATH = " + path + "   prefix = " + prefix)
    setFileOwnership(path, userAndGroup)
    path
  }


  /**
    * Check if a file exists.
    * n.b. Only Exists so user code does not need to import java.nio.Files to reduce likelyhood of ownership mistakes.
    *
    * @param path The target path
    * @return True if the file exists, false otherwise
    */
  def exists(path: Path): Boolean = {
    Files.exists(path)
  }

  def delete(path: Path): Unit = {
    Files.delete(path)
  }

  def createDirectories(dir: Path)(implicit userAndGroup: UserAndGroup): Unit = {
    val parentDir = dir.getParent
    if(!Files.exists(parentDir))
      createDirectories(parentDir)

    if(!Files.exists(dir))
      Files.createDirectory(dir)
    setFileOwnership(dir, userAndGroup)

  }


  /**
    * Read a file into a string
    *
    * @param src The source file to read
    * @return Some(content) if the file exists, None otherwise
    */
  def slurp(src: Path): Option[String] = {
    if (Files.exists(src)) Some(Files.readAllLines(src).toArray.mkString(nl))
    else None
  }

  /** Set file ownership if the ownership has been defined
    *
    * @param file The target file
    * @param userAndGroup The user and group configuration to set
    */
  def setFileOwnership(file: Path, userAndGroup: UserAndGroup): Unit = {
    userAndGroup match {
      case SomeUserAndGroup(user, group) =>
        import java.nio.file.Files
        import java.nio.file.attribute.PosixFileAttributeView

        val userPrincipal = file.getFileSystem.getUserPrincipalLookupService.lookupPrincipalByName(user)
        val groupPrincipal = file.getFileSystem.getUserPrincipalLookupService.lookupPrincipalByGroupName(group)
        Files.setOwner(file, userPrincipal)
        Files.getFileAttributeView(file, classOf[PosixFileAttributeView]).setGroup(groupPrincipal)
      case NoUserAndGroup =>
      // do nothing
    }

  }

  def write(dst: Path,
            content: String,
            truncate: Boolean = false,
            append: Boolean = false
           )(implicit userAndGroup: UserAndGroup): Unit = {
    if (!Files.exists(dst)) {
      Files.write(dst, content.getBytes, StandardOpenOption.CREATE_NEW)
    } else if (truncate) {
      Files.write(dst, content.getBytes, StandardOpenOption.TRUNCATE_EXISTING)
    } else if (append) {
      Files.write(dst, content.getBytes, StandardOpenOption.APPEND)
    }

    setFileOwnership(dst, userAndGroup)
    ()
  }

  /**
    * Write the pid of this process to a file in the cwd named RUNNING_PID
    * @return The pid of this process as a string
    */
  def writeRunningPid(): String = {
    val pid = ManagementFactory.getRuntimeMXBean.getName.split("@").head
    val pidFile = Paths.get("RUNNING_PID")
    Files.write(pidFile, pid.getBytes(StandardCharsets.UTF_8))
    sys.addShutdownHook {
      Files.delete(pidFile)
    }
    pid
  }
}
