      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1482.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1482.CPv  $
      *
      *   Rev 2.6   Aug 30 2010 13:32:56   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.5   Aug 30 2010 10:42:06   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.4   Aug 27 2010 15:54:06   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.3   Aug 27 2010 15:36:48   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.2   Dec 10 2007 13:12:50   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   20 Mar 2001 15:26:24   T13EX21
      *APA00131 - Neuerstellung
      *
      *   Rev 2.0   09 Jan 2001 10:22:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1482                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<VORARBEITEN ÜBERNAHME COBA-PROVISIONEN>> *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    12.03.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0131!12.03.01!2.01! NEUERSTELLUNG             ! STORCH   *
      * APA00622!07.08.07!2.02! ANPASSUNG FÜR ALLFINANZ   ! STORCH   *
      * APA00994!25.01.10!2.03! ANPASSUNG FÜR DREBA       ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1454                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1482.
           05 PI-DRPA1482-SATZ.
              30 PI-DRPA1482-SATZ-ART         PIC X(001).
                 COPY DRPA1472.

           05 PI-DRPA1482-A-SATZ   REDEFINES PI-DRPA1482-SATZ.
              10 PI-DRPA1482-A-SATZART        PIC X(001).
              10 PI-DRPA1482-A-ABSENDER       PIC X(045).
              10 PI-DRPA1482-A-ERSTELL-DAT    PIC S9(09) COMP-3.
070807        10 PI-DRPA1482-A-KOP-KD-NR      PIC S9(11) COMP-3.
070807*       10 FILLER                       PIC X(548).
070807*       10 FILLER                       PIC X(542).
250110        10 FILLER                       PIC X(543).

      ******* ENDE COPY-MEMBER **************************************
