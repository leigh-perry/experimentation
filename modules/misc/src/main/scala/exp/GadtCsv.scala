package exp

import cats.implicits._

object GadtCsv {
  /*
    data CSV index where
      NamedCsv    :: [String] -> [[String]] -> CSV String
      NumberedCsv ::             [[String]] -> CSV Int

    data CSVType i where
      Named :: CSVType String
      Numbered :: CSVType Int

    decode :: CSVType i -> String -> Maybe (CSV i)
    decode Named s = case splitOn ',' <$> lines s of
        (h:xs) -> Just $ NamedCsv h xs
        _ -> Nothing
    decode Numbered s = Just . NumberedCsv . fmap (splitOn ',') . lines $ s

    getColumnByIndex :: i -> CSV i -> Maybe [String]
    getColumnByIndex  columnName (NamedCsv headers rows) = do
        columnIndex <- elemIndex columnName headers
        traverse (safeIndex columnIndex) rows
    getColumnByIndex n (NumberedCsv rows) = traverse (safeIndex n) rows

    getColumnByNumber :: Int -> CSV i -> Maybe [String]
    getColumnByNumber n (NamedCsv _ rows) = traverse (safeIndex n) rows
    getColumnByNumber n (NumberedCsv rows) = traverse (safeIndex n) rows

    getHeaders :: CSV String -> [String]
    getHeaders (NamedCsv headers _) = headers
   */

  /**
   * @tparam I index type
   */
  sealed trait Csv[I]
  object Csv {
    final case class NamedCsv(headers: List[String], rowsOfColumns: List[List[String]]) extends Csv[String]
    final case class NumberedCsv(rowsOfColumns: List[List[String]]) extends Csv[Int]
  }

  sealed trait CsvType[I]
  object CsvType {
    case object Named extends CsvType[String]
    case object Numbered extends CsvType[Int]
  }

  def decode[I](ct: CsvType[I], s: String): Option[Csv[I]] = {
    def explode = s.split("\n").toList.map(_.split(",").toList)

    ct match {
      case CsvType.Named =>
        explode match {
          case ::(head, tail) => Some(Csv.NamedCsv(head, tail))
          case Nil => None
        }

      case CsvType.Numbered =>
        Some(Csv.NumberedCsv(explode))
    }
  }

  def getColumnByIndex[I](index: I, csv: Csv[I]): Option[List[String]] =
    csv match {
      case Csv.NamedCsv(headers, rowsOfColumns) =>
        for {
          columnIndex <- {
            val i = headers.indexOf(index)
            Option.unless(i === -1)(i)
          }
          r <- rowsOfColumns.traverse(_.get(columnIndex.toLong))
        } yield r

      case Csv.NumberedCsv(rowsOfColumns) =>
        rowsOfColumns.traverse(_.get(index.toLong))
    }

  def getColumnByNumber[I](index: Int, csv: Csv[I]): Option[List[String]] =
    csv match {
      case Csv.NamedCsv(_, rowsOfColumns) => rowsOfColumns.traverse(_.get(index.toLong))
      case Csv.NumberedCsv(rowsOfColumns) => rowsOfColumns.traverse(_.get(index.toLong))
    }

  def getHeaders(csv: Csv[String]): List[String] =
    csv match {
      case Csv.NamedCsv(headers, _) => headers
    }
}
