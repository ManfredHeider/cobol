      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0200.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0200.CPv  $
      *
      *   Rev 2.5   Jul 16 2015 15:10:36   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.4   Jun 10 2008 10:29:46   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.3   03 Nov 2006 09:56:46   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Oct 18 1999 08:37:42   T13EX21
      *FPA00091 - Erweiterung PI-Bereich
      *
      *   Rev 2.1   Jun 29 1999 13:47:58   T13EX21
      *APA00020 - Erweiterung PO-Bereich
      *
      *   Rev 2.0   Feb 27 1999 14:28:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:51:10   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:08:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0200                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0200 IM ZWPA0004        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    23.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!23.01.97!    ! NEU                       ! AUERBACH *
      * APA00012!22.06.99!    ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * FPA00091!29.09.99!    ! ERWEITERUNG PI-BEREICH    ! STORCH   *
      * APA00452!02.11.06!2.3 ! UMBENENNUNG WG. 3-STELL.  ! WANG     *
      *         !        !    ! PRODUKTE                  !          *
      * APA00712!12.03.08!2.4 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!02.06.15!2.5 ! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0200: PRODUKT-BEZ. ERMITTELN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0200.
           05  PI-DRPA0200-DATEN.
290999         10 PI-DRPA0200-PGEB-GES         PIC S9(004) COMP.
021106*        10 PI-DRPA0200-PROD-SCHL        PIC S9(004) COMP.
021106         10 PI-DRPA0200-PROD-PROD-SCHL   PIC S9(004) BINARY.
290999         10 PI-DRPA0200-STICHTAG         PIC  X(010).
      *
       01  PO-DRPA0200.
           05  PO-DRPA0200-RC                  PIC  X(003).
               88 PO-DRPA0200-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0200-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0200-DATEN.
               10 PO-DRPA0200-PROD-BEZ-KURZ    PIC  X(010).
220699         10 PO-DRPA0200-PROD-BEZ         PIC  X(040).
120308         10 PO-DRPA0200-KNR-MANDANT      PIC S9(004) BINARY.
020615         10 PO-DRPA0200-PROD-UST-SATZ    PIC S9(003)V9(003).
      *

