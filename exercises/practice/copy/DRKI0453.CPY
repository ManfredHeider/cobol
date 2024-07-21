      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0453.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0453.CPv  $
      *
      *   Rev 2.2   Sep 15 1999 13:09:42   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 11:56:04   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:24:14   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:55:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0453                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.09.99/08.16.46
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
      *    FÜR DIE FUNKTION DRKI0453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0453.
           05  PI-DRKI0453-KEY.


             10 PI-DRKI0453-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0453-KFMVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0453.
           05  PO-DRKI0453-RC                 PIC  X(003).
               88 PO-DRKI0453-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0453-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0453-DATEN.


             10 PO-DRKI0453-KFMBEITRSATZHAFTPF   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI0453-KFMDECKSCHLHPFLID   PIC X(2).
             10 PO-DRKI0453-KFMHAFTPFLBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0453-KFMHAFTPFLREGIOKL   PIC X(3).
             10 PO-DRKI0453-KFMHAFTPFLSCHADFID   PIC S9(4) COMP.
             10 PO-DRKI0453-N-KFMHFTPFLSCHDFD   PIC S9(4) COMP.
             10 PO-DRKI0453-KFMHPFBEITRAGWKZ   PIC X(01).
             10 PO-DRKI0453-KFMTARBEZEICHNUNG   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
