      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1037.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1037.CPv  $
      *
      *   Rev 2.1   Mar 10 2014 08:51:46   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG, PO21,PO23,PF14, PF15, PO25,PO26
      *
      *   Rev 2.0   May 26 1999 13:42:36   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1037                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1037                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    12.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.05.98! 01 ! NEU                       ! BOHLINGER*
      * APA01526!06.03.14!2.01! ERWEITERUNG FOLGE-NR      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1037
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1037 DES BUFFER-OZP ZYPA0019 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PO22, AUSKUNFT PROVISIONSBEWEGUNGEN
      *                                         JE PRODUKT
      ****************************************************************
      *
       01  PI-DRPA1037.
           05  PI-DRPA1037-UFKT                    PIC  9(002).
           05  PI-DRPA1037-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1037-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1037-PRINTE                  PIC S9(015)
                                                   PACKED-DECIMAL.
           05  PI-DRPA1037-AUSWAHL-KRITERIEN.
               10 PI-DRPA1037-PGEB-GES             PIC 9(002).
               10 PI-DRPA1037-VNR                  PIC X(015).
               10 PI-DRPA1037-FAELLIG-VON          PIC X(010).
               10 PI-DRPA1037-FAELLIG-BIS          PIC X(010).
               10 PI-DRPA1037-S-FAELLIG            PIC X(001).
060314*        10 PI-DRPA1037-FOLGE-NR             PIC 9(002).
060314         10 PI-DRPA1037-FOLGE-NR             PIC 9(003).
               10 PI-DRPA1037-S-FOLGE-NR           PIC X(001).

      *
       01  PO-DRPA1037.
           05  PO-DRPA1037-RC                      PIC  X(03).
               88 PO-DRPA1037-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1037-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1037-RC-LEER                      VALUE 'LER'.


          05 PO-DRPA1037-MELDUNG.
             10 PO-DRPA1037-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1037-MELDUNG-NR         PIC S9(004) BINARY.

           05  PO-DRPA1037-ANZAHL-ERG-ZEILEN       PIC  9(05).

           05  PO-DRPA1037-ERGEBNISZEILE.
               COPY DRPA0934.
      *
      ******* ENDE COPY-MEMBER **************************************
