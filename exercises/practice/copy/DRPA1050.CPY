      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1050.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1050.CPv  $
      *
      *   Rev 2.5   Oct 10 2016 10:03:00   t13ex67
      *APA02280: Dialoganpassungen
      *
      *   Rev 2.4   Sep 26 2016 11:18:16   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.3   Jun 19 2012 09:27:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:52:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:57:48   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 11 1999 08:15:16   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1050                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1050                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    24.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.08.98!    ! NEU                       ! D.WILHELM*
      * APA00622!30.07.07!2.01! NEUES AUSWAHLKRIT.KNR-VGES! STORCH   *
      * APA01101!23.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * ???     !20.06.16!02.03 PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1050
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1050.
           05  PI-DRPA1050-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
      *
           05  PI-DRPA1050-AUSWAHL-KRITERIEN.
               10 PI-DRPA1050-PGEB-GES         PIC  9(02).
               10 PI-DRPA1050-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA1050-ABR-DAT-VON      PIC  X(10).
               10 PI-DRPA1050-S-ABR-DAT-VON    PIC  X(01).
               10 PI-DRPA1050-ABR-DAT-BIS      PIC  X(10).
               10 PI-DRPA1050-S-ABR-DAT-BIS    PIC  X(01).
               10 PI-DRPA1050-MANDANT          PIC  9(02).
               10 PI-DRPA1050-S-MANDANT        PIC  X(01).
200616         10 PI-DRPA1050-PRODUKT          PIC  9(04).
200616         10 PI-DRPA1050-S-PRODUKT        PIC  X(01).
      *
       01  PO-DRPA1050.
           05  PO-DRPA1050-RC                  PIC  X(03).
               88 PO-DRPA1050-RC-OK                 VALUE 'OK '.
               88 PO-DRPA1050-RC-EOF                VALUE 'EOF'.
               88 PO-DRPA1050-RC-LEER               VALUE 'LER'.
      *
           05  PO-DRPA1050-ERGEBNISZEILE.
               10 PO-DRPA1050-ANZ-FEHL         PIC  S9(07) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


