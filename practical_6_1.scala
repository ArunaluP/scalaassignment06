import scala.io.StdIn.readLine
import scala.util.Try

object StudentRecordsManager {

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry()
    printStudentRecord(studentInfo)
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    val name = readLine("Enter student name: ")
    val marks = readLine("Enter student marks: ").toInt
    val totalMarks = readLine("Enter total possible marks: ").toInt

    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(s"Percentage: $percentage%")
    println(s"Grade: $grade")
  }

  def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      return (false, Some("Name cannot be empty."))
    }
    val marksInt = Try(marks.toInt).getOrElse(-1)
    val totalMarksInt = Try(totalMarks.toInt).getOrElse(-1)

    if (marksInt < 0 || totalMarksInt < 0) {
      return (false, Some("Marks and total marks must be positive integers."))
    }
    if (marksInt > totalMarksInt) {
      return (false, Some("Marks obtained cannot exceed total possible marks."))
    }

    (true, None)
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!isValid) {
      val name = readLine("Enter student name: ")
      val marks = readLine("Enter student marks: ")
      val totalMarks = readLine("Enter total possible marks: ")

      val (valid, errorMessage) = validateInput(name, marks, totalMarks)

      if (valid) {
        val percentage = (marks.toInt.toDouble / totalMarks.toInt) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        studentInfo = (name, marks.toInt, totalMarks.toInt, percentage, grade)
        isValid = true
      } else {
        println(s"Error: ${errorMessage.get}")
      }
    }

    studentInfo
  }
}
