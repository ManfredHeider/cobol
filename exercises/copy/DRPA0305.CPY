      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0305.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0305.CPv  $
      *
      *   Rev 2.6   05 Mar 2018 11:36:26   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.5   Sep 26 2016 11:39:30   t13ex67
      *APA02280: Dialoganpassungen PG11
      *
      *   Rev 2.4   Jul 16 2015 15:07:00   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:22:54   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:47:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 08:58:08   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:28:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:17:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0305                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0305                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !03.06.97!    ! NEU                       ! SCHRIEBER*
      * APA00622!23.05.07!2.1 ! NEUES FELD KNR-VGES       ! STORCH   *
      * APA01101!23.02.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!24.04.15!2.3 ! NEUES FELD UST-BETRAG     ! WANG     *
      * APA02280!05.07.16!2.4 ! NEU: PI-SORTIERUNG        ! STORCH   *
      * APA02481!10.07.17!2.5 ! NEU: PI/PO-BUCHUNGSDATUM  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0305
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0305.
           05  PI-DRPA0305-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
      *
           05  PI-DRPA0305-AUSWAHL-KRITERIEN.
               10 PI-DRPA0305-PGEB-GES         PIC  9(02).
               10 PI-DRPA0305-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA0305-DTA-ART          PIC  9(03).
               10 PI-DRPA0305-S-DTA-ART        PIC  X(01).
               10 PI-DRPA0305-VSTAT            PIC  X(04).
               10 PI-DRPA0305-S-VSTAT          PIC  X(01).
               10 PI-DRPA0305-UEBDAT-VON       PIC  X(10).
               10 PI-DRPA0305-UEBDAT-BIS       PIC  X(10).
               10 PI-DRPA0305-MANDANT          PIC  9(03).
               10 PI-DRPA0305-S-MANDANT        PIC  X(01).
050716         10 PI-DRPA0305-SORTIERUNG       PIC  9(01).
100717         10 PI-DRPA0305-BUCHDAT-VON      PIC  X(10).
100717         10 PI-DRPA0305-BUCHDAT-BIS      PIC  X(10).
100717         10 PI-DRPA0305-S-BUCHDAT        PIC  X(01).
      *
       01  PO-DRPA0305.
           05  PO-DRPA0305-RC                 PIC  X(003).
               88 PO-DRPA0305-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0305-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0305-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0305-ERGEBNISZEILE.
               10 PO-DRPA0305-DTA-ID           PIC  9(015).
               10 PO-DRPA0305-PGEB-GES         PIC  9(007).
               10 PO-DRPA0305-DTA-ART          PIC  9(007).
               10 PO-DRPA0305-VSTAT            PIC  X(004).
               10 PO-DRPA0305-ERSTELL-DAT      PIC  X(010).
               10 PO-DRPA0305-TRANSF-DAT       PIC  X(010).
               10 PO-DRPA0305-DTA-ANZ-SAETZE   PIC S9(009)
                                                     USAGE IS BINARY.
               10 PO-DRPA0305-KONTR-WERT       PIC S9(009)V99
                                                     PACKED-DECIMAL.
               10 PO-DRPA0305-LAST-UPDATE      PIC  X(026).
               10 PO-DRPA0305-MANDANT          PIC  9(003).
240415         10 PO-DRPA0305-UST-BETRAG       PIC S9(009)V99
240415                                               PACKED-DECIMAL.
100717         10 PO-DRPA0305-BUCH-DAT         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************



