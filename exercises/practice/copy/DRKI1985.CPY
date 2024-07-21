      *****************************************************************
      *    COPY-MEMBER: DRKI1985                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1985                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  08.11.2006/13:38:11
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1985
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1985.
           05  PI-DRKI1985-KEY.


             10 PI-DRKI1985-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1985-KFMVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1985.
           05  PO-DRKI1985-RC                 PIC  X(003).
               88 PO-DRKI1985-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1985-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1985-DATEN.


             10 PO-DRKI1985-KFMBEITRSATZHAFTPF   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1985-N-KFMBTRSTZHFTPF   PIC S9(4) COMP.
             10 PO-DRKI1985-KFMDECKSCHLHPFLID   PIC X(2).
             10 PO-DRKI1985-KFMDECKUMFANG.
                15 PO-DRKI1985-KFMDECKUMFANG-LEN   PIC S9(4) COMP.
                15 PO-DRKI1985-KFMDECKUMFANG-TEXT   PIC X(30).
                
             10 PO-DRKI1985-KFMHAFTPFLBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1985-N-KFMHFTPFLBTRG   PIC S9(4) COMP.
             10 PO-DRKI1985-KFMHAFTPFLREGIOKL   PIC X(3).
             10 PO-DRKI1985-KFMHAFTPFLSCHADFID   PIC S9(4) COMP.
             10 PO-DRKI1985-N-KFMHFTPFLSCHDFD   PIC S9(4) COMP.
             10 PO-DRKI1985-KFMHPFBEITRAGWKZ   PIC X(01).
             10 PO-DRKI1985-KFMTARBEZEICHNUNG   PIC X(15).
             

      *
      ******* ENDE COPY-MEMBER **************************************
