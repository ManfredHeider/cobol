      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0651.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0651.CPv  $
      *
      *   Rev 2.1   Dec 08 1999 10:33:40   T13EX19
      *aki00058 - neu
      *
      *   Rev 2.0   Feb 27 1999 12:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:27:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *  Select All
      *****************************************************************
      *    COPY-MEMBER: DRKI0651                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0651                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  03.11.99/14.37.03
      *   !!!!HÄNDISCH KORRIGIERT
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
      *    FšR DIE FUNKTION DRKI0651
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0651.
           05  PI-DRKI0651-KEY.


             10 PI-DRKI0651-SP-PC-NR   PIC S9(9) COMP.

      *
       01  PO-DRKI0651.
           05  PO-DRKI0651-RC                         PIC  X(003).
               88 PO-DRKI0651-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0651-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0651-DATEN.


             10 PO-DRKI0651-SP-ABAKUSDATUM           PIC X(10).
             10 PO-DRKI0651-NI-ABAKUSDATUM           PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ABAKUSKZ              PIC X(1).
             10 PO-DRKI0651-NI-ABAKUSKZ              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ALKZ                  PIC X(3).
             10 PO-DRKI0651-NI-ALKZ                  PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ANREDEKZ              PIC S9(4) COMP.
             10 PO-DRKI0651-NI-ANREDEKZ              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-DASIKZ                PIC X(1).
             10 PO-DRKI0651-NI-DASIKZ                PIC S9(4) COMP.
             10 PO-DRKI0651-SP-DFUE-ART              PIC X(1).
             10 PO-DRKI0651-NI-DFUE-ART              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-DIALOGKZ              PIC X(1).
             10 PO-DRKI0651-NI-DIALOGKZ              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ISDN-SUBADR           PIC X(1).
             10 PO-DRKI0651-NI-ISDN-SUBADR           PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ISDN1                 PIC X(20).
             10 PO-DRKI0651-NI-ISDN1                 PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ISDN2                 PIC X(20).
             10 PO-DRKI0651-NI-ISDN2                 PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ISDN3                 PIC X(20).
             10 PO-DRKI0651-NI-ISDN3                 PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LALLGDFUEDAT          PIC X(10).
             10 PO-DRKI0651-NI-LALLGDFUEDAT          PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LEADAT                PIC X(10).
             10 PO-DRKI0651-NI-LEADAT                PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LETZTEDASIDAT         PIC X(10).
             10 PO-DRKI0651-NI-LETZTEDASIDAT         PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LGESDFUEDAT           PIC X(10).
             10 PO-DRKI0651-NI-LGESDFUEDAT           PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LOESCHDATUM           PIC X(10).
             10 PO-DRKI0651-NI-LOESCHDATUM           PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LPRGADFUEDAT          PIC X(10).
             10 PO-DRKI0651-NI-LPRGADFUEDAT          PIC S9(4) COMP.
             10 PO-DRKI0651-SP-LREPLDFUEDAT          PIC X(10).
             10 PO-DRKI0651-NI-LREPLDFUEDAT          PIC S9(4) COMP.
             10 PO-DRKI0651-SP-NAME                  PIC X(30).
             10 PO-DRKI0651-NI-NAME                  PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ORT                   PIC X(30).
             10 PO-DRKI0651-NI-ORT                   PIC S9(4) COMP.
             10 PO-DRKI0651-SP-PC-LINIE              PIC X(2).
             10 PO-DRKI0651-NI-PC-LINIE              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-PC-NR-NEU             PIC S9(9) COMP.
             10 PO-DRKI0651-NI-PC-NR-NEU             PIC S9(4) COMP.
             10 PO-DRKI0651-SP-PCTYP                 PIC X(01).
             10 PO-DRKI0651-SP-PLZ                   PIC X(15).
             10 PO-DRKI0651-NI-PLZ                   PIC S9(4) COMP.
             10 PO-DRKI0651-SP-REPLSPERREZAEHL       PIC S9(4) COMP.
             10 PO-DRKI0651-NI-REPLSPERREZAEHL       PIC S9(4) COMP.
             10 PO-DRKI0651-SP-STRASSE               PIC X(45).
             10 PO-DRKI0651-NI-STRASSE               PIC S9(4) COMP.
             10 PO-DRKI0651-SP-TARIFZONE             PIC S9(4) COMP.
             10 PO-DRKI0651-NI-TARIFZONE             PIC S9(4) COMP.
             10 PO-DRKI0651-SP-TELEFAX               PIC X(15).
             10 PO-DRKI0651-NI-TELEFAX               PIC S9(4) COMP.
             10 PO-DRKI0651-SP-TELEFON               PIC X(15).
             10 PO-DRKI0651-NI-TELEFON               PIC S9(4) COMP.
             10 PO-DRKI0651-SP-TELEFONANLAGE         PIC X(30).
             10 PO-DRKI0651-NI-TELEFONANLAGE         PIC S9(4) COMP.
             10 PO-DRKI0651-SP-TERMINALID            PIC X(4).
             10 PO-DRKI0651-NI-TERMINALID            PIC S9(4) COMP.
             10 PO-DRKI0651-SP-VB-ID                 PIC X(13).
             10 PO-DRKI0651-NI-VB-ID                 PIC S9(4) COMP.
             10 PO-DRKI0651-SP-VB-NR                 PIC S9(9) COMP.
             10 PO-DRKI0651-NI-VB-NR                 PIC S9(4) COMP.
             10 PO-DRKI0651-SP-VB-ROLLEN-KZ          PIC X(1).
             10 PO-DRKI0651-NI-VB-ROLLEN-KZ          PIC S9(4) COMP.
             10 PO-DRKI0651-SP-VORNAME               PIC X(15).
             10 PO-DRKI0651-NI-VORNAME               PIC S9(4) COMP.
             10 PO-DRKI0651-SP-VW-PC-NR              PIC S9(9) COMP.
             10 PO-DRKI0651-NI-VW-PC-NR              PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ZWDASIMAXTAGE         PIC S9(4) COMP.
             10 PO-DRKI0651-NI-ZWDASIMAXTAGE         PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ZWDFUEMAXTAGE         PIC S9(4) COMP.
             10 PO-DRKI0651-NI-ZWDFUEMAXTAGE         PIC S9(4) COMP.
             10 PO-DRKI0651-SP-ZWPROGAUSTZAEHL       PIC S9(4) COMP.
             10 PO-DRKI0651-NI-ZWPROGAUSTZAEHL       PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
