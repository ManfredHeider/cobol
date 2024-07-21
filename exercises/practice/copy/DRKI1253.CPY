      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1253.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1253.CPv  $
      *
      *   Rev 2.1   May 03 2006 09:07:42   t13ex39
      *AKI00215: Umstellung auf EURO KFZMOPEDVERS
      *
      *   Rev 2.0   05 Jul 2001 17:06:50   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1253                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1253                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.44.02
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1253
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1253.
           05  PI-DRKI1253-KEY.


             10 PI-DRKI1253-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1253-KFMVERTRAGSID   PIC X(13).


           05  PI-DRKI1253-DATEN.


             10 PI-DRKI1253-KFMBEITRSATZHAFTPF   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1253-N-KFMBTRSTZHFTPF   PIC S9(4) COMP.
             10 PI-DRKI1253-KFMDECKSCHLHPFLID   PIC X(2).
             10 PI-DRKI1253-KFMDECKUMFANG.
                15 PI-DRKI1253-KFMDECKUMFANG-LEN   PIC S9(4) COMP.
                15 PI-DRKI1253-KFMDECKUMFANG-TEXT   PIC X(30).

             10 PI-DRKI1253-KFMHAFTPFLBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1253-N-KFMHFTPFLBTRG   PIC S9(4) COMP.
             10 PI-DRKI1253-KFMHAFTPFLREGIOKL   PIC X(3).
             10 PI-DRKI1253-KFMHAFTPFLSCHADFID   PIC S9(4) COMP.
             10 PI-DRKI1253-N-KFMHFTPFLSCHDFD   PIC S9(4) COMP.
             10 PI-DRKI1253-KFMHPFBEITRAGWKZ   PIC X(01).
             10 PI-DRKI1253-KFMTARBEZEICHNUNG   PIC X(15).


      *
       01  PO-DRKI1253.
           05  PO-DRKI1253-RC                 PIC  X(003).
               88 PO-DRKI1253-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1253-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
