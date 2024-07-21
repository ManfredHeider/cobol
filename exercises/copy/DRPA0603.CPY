      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0603.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0603.CPv  $
      *
      *   Rev 2.5   Jul 16 2015 15:07:30   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.4   Jun 19 2012 09:24:06   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:49:10   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 09:46:08   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Nov 02 2006 14:51:32   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 27 1999 12:36:28   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:54:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0603 FUNKTION PM ZUKÜNFTIG MARKIEREN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. JÄGER-SOKOLUK                                *
      *    DATUM:    09.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97!01.01!NEU                       ! JÄGER    *
      * APA00012!27.04.98!01.02!VSTATUS EINFÜGEN          ! JÄGER    *
      * APA00012!28.05.98!01.03!NEUE ATTRIBUTE EINGEFÜGT  ! JÄGER    *
      * APA00452!01.11.06! 2.1 !DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA00622!27.07.07! 2.2 ! NEUES ATTRIBUT KNR-VGES  ! STORCH   *
      * APA01101!01.03.12! 2.3 !VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !     !*-ZEILEN ENTFERNT         !          *
      * APA01932!07.05.15! 2.5 !UST-BETRAG HINZU          ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0603                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0603.
          05 PI-DRPA0603-DATEN.
             10 PI-DRPA0603-PM-ID            PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0603-PM-VERSION       PIC S9(004) BINARY.
             10 PI-DRPA0603-PM-HERKUNFT      PIC S9(004) BINARY.
             10 PI-DRPA0603-PM-PRINTE        PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0603-PM-PIN           PIC S9(009) BINARY.
             10 PI-DRPA0603-PM-VNR           PIC  X(015).
             10 PI-DRPA0603-PM-PGEB-GES      PIC S9(004) BINARY.
             10 PI-DRPA0603-PM-KUNDE-NAME    PIC  X(025).
             10 PI-DRPA0603-PM-FAELLIG-VON   PIC  X(010).
             10 PI-DRPA0603-PM-GSMT-PROV     PIC S9(009)V99 COMP-3.
             10 PI-DRPA0603-PM-PG-ABR-DAT    PIC  X(010).
             10 PI-DRPA0603-PROD-PROD-SCHL-1 PIC S9(004)  BINARY.
             10 PI-DRPA0603-PROD-PROD-SCHL-2 PIC S9(004)  BINARY.
             10 PI-DRPA0603-PROD-PROD-SCHL-3 PIC S9(004)  BINARY.
             10 PI-DRPA0603-PM-PROD-SCHL-KZ  PIC X.
             10 PI-DRPA0603-PM-VSTAT         PIC  X(004).
             10 PI-DRPA0603-PM-VSTAT-UNR     PIC S9(004) BINARY.
             10 PI-DRPA0603-PM-KNR-MANDANT   PIC S9(004) BINARY.
070515       10 PI-DRPA0603-PM-UST-BETRAG    PIC S9(009)V99 COMP-3.
      *
       01 PO-DRPA0603.
          05 PO-DRPA0603-RC                  PIC  X(003).
             88 PO-DRPA0603-RC-OK                 VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************

