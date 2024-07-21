      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0444.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0444.CPv  $
      *
      *   Rev 2.2   Sep 15 1999 13:09:38   T13EX39
      *AKI00049 WKZ eingebaut
      *
      *   Rev 2.1   Mar 10 1999 11:55:44   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:23:54   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   28 Jul 1998 09:54:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0444                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0444                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.09.99/08.16.47
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
      *    FÜR DIE FUNKTION DRKI0444
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0444.
           05  PI-DRKI0444-KEY.


             10 PI-DRKI0444-KFMVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0444-KFMVERTRAGSID   PIC X(13).


           05  PI-DRKI0444-DATEN.


             10 PI-DRKI0444-KFMBEITRSATZHAFTPF   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0444-KFMDECKSCHLHPFLID   PIC X(2).
             10 PI-DRKI0444-KFMHAFTPFLBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0444-KFMHAFTPFLREGIOKL   PIC X(3).
             10 PI-DRKI0444-KFMHAFTPFLSCHADFID   PIC S9(4) COMP.
             10 PI-DRKI0444-N-KFMHFTPFLSCHDFD   PIC S9(4) COMP.
             10 PI-DRKI0444-KFMHPFBEITRAGWKZ   PIC X(01).
             10 PI-DRKI0444-KFMTARBEZEICHNUNG   PIC X(15).


      *
       01  PO-DRKI0444.
           05  PO-DRKI0444-RC                 PIC  X(003).
               88 PO-DRKI0444-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0444-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
