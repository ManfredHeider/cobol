      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1278.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1278.CPv  $
      *
      *   Rev 2.1   May 03 2006 09:07:50   t13ex39
      *AKI00215: Umstellung auf EURO KFZMOPEDVERS
      *
      *   Rev 2.0   05 Jul 2001 17:07:16   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1278                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1278                    *
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
      *    FšR DIE FUNKTION DRKI1278
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1278.
           05  PI-DRKI1278-KEY.


             10 PI-DRKI1278-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1278-KFMVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1278.
           05  PO-DRKI1278-RC                 PIC  X(003).
               88 PO-DRKI1278-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1278-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1278-DATEN.


             10 PO-DRKI1278-KFMBEITRSATZHAFTPF   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1278-N-KFMBTRSTZHFTPF   PIC S9(4) COMP.
             10 PO-DRKI1278-KFMDECKSCHLHPFLID   PIC X(2).
             10 PO-DRKI1278-KFMDECKUMFANG.
                15 PO-DRKI1278-KFMDECKUMFANG-LEN   PIC S9(4) COMP.
                15 PO-DRKI1278-KFMDECKUMFANG-TEXT   PIC X(30).

             10 PO-DRKI1278-KFMHAFTPFLBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1278-N-KFMHFTPFLBTRG   PIC S9(4) COMP.
             10 PO-DRKI1278-KFMHAFTPFLREGIOKL   PIC X(3).
             10 PO-DRKI1278-KFMHAFTPFLSCHADFID   PIC S9(4) COMP.
             10 PO-DRKI1278-N-KFMHFTPFLSCHDFD   PIC S9(4) COMP.
             10 PO-DRKI1278-KFMHPFBEITRAGWKZ   PIC X(01).
             10 PO-DRKI1278-KFMTARBEZEICHNUNG   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
