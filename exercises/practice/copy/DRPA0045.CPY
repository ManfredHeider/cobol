      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0045.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0045.CPv  $
      *
      *   Rev 2.3   Jul 16 2015 15:06:22   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.2   Jun 10 2008 10:28:50   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Jan 04 2000 13:04:48   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:27:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:51:10   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0045  FUNKTION READ STANDARD-OZP         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    14.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !14.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * APA00068!18.11.99!2.01!NEUES KEY-FELD: PROD-BEGINN! BOHLINGER*
      * APA00712!11.03.08!2.02! ERWEITERUNG UM MANDANT    ! STORCH   *
      * APA01932!01.04.15!2.3 !ERW. PROD-UST-SATZ         ! WANG     *
      *         !        !    !KNR-KZ-SOND-LST            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0045                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PROD TBPA0004                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0045.
          05 PI-DRPA0045-PROD-KEY.
             10 PI-DRPA0045-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0045-PROD-PROD-SCHL   PIC S9(004) BINARY.
181199       10 PI-DRPA0045-PROD-BEGINN      PIC  X(010).
      *
       01 PO-DRPA0045.
          05 PO-DRPA0045-RC                  PIC  X(003).
             88 PO-DRPA0045-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0045-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0045-PROD-DATEN.
181199*      10 PO-DRPA0045-PROD-BEGINN      PIC  X(010).
             10 PO-DRPA0045-PROD-ENDE        PIC  X(010).
             10 PO-DRPA0045-PROD-BEZ         PIC  X(040).
             10 PO-DRPA0045-PROD-BEZ-KURZ    PIC  X(010).
          05 PO-DRPA0045-PROD-LAST-UPDATE    PIC  X(026).
          05 PO-DRPA0045-PROD-KENNUNG        PIC  X(008).
110308    05 PO-DRPA0045-KNR-MANDANT         PIC S9(004) BINARY.
010415    05 PO-DRPA0045-PROD-UST-SATZ       PIC S9(003)V999 COMP-3.
010415    05 PO-DRPA0045-KNR-KZ-SOND-LST     PIC S9(004) BINARY.
      *
      ****** Ende Copy-Member ****************************************

