      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0776.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0776.CPv  $
      *
      *   Rev 2.3   Jun 10 2008 10:31:08   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.2   03 Nov 2006 09:56:56   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jan 04 2000 13:09:20   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:46:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0776                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0776 IM ZWPA0004        *
      *                  ERMITTELT AKTUELLE PRODUKTBEZEICHNUNG ZUR    *
      *                  GESELLSCHAFT UND DEM PRODUKTSCHLÜSSEL        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    18.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!18.11.99!2.01! NEU                       ! BOHLINGER*
      * APA00452!02.11.06!2.02! UMBENENNUNG WG. 3-STELL.  ! WANG     *
      *         !        !    ! PRODUKTE                  !          *
      * APA00712!12.03.08!2.03! ERWEITERUNG PO-BEREICH    ! STORCH   *
      *         !        !    ! UM MANDANT                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0776: PRODUKT-BEZ. ERMITTELN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0776.
           05  PI-DRPA0776-DATEN.
               10 PI-DRPA0776-PGEB-GES         PIC S9(004) COMP.
021106*        10 PI-DRPA0776-PROD-SCHL        PIC S9(004) COMP.
021106         10 PI-DRPA0776-PROD-PROD-SCHL   PIC S9(004) BINARY.
      *
       01  PO-DRPA0776.
           05  PO-DRPA0776-RC                  PIC  X(003).
               88 PO-DRPA0776-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0776-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0776-DATEN.
               10 PO-DRPA0776-PROD-BEZ-KURZ    PIC  X(010).
               10 PO-DRPA0776-PROD-BEZ         PIC  X(040).
               10 PO-DRPA0776-PROD-BEGINN      PIC  X(010).
               10 PO-DRPA0776-PROD-ENDE        PIC  X(010).
120308         10 PO-DRPA0776-KNR-MANDANT      PIC S9(004) BINARY.
      *
