      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1039.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1039.CPv  $
      *
      *   Rev 2.5   Sep 26 2016 11:17:34   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.4   Jul 21 2015 10:45:50   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:26:40   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:30   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:34:54   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 11 1999 08:15:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1039  FUNKTION SUCHE ALLE EINTRÄGE ZU    *
      *                                    EINEM VSTATUS             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                       *
      *    DATUM:    15.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.08.98!01.01! NEUERSTELLUNG             !SCHRIEBER*
      * APA00622!30.07.07!02.01! NEUES AUSWAHLKRIT.KNR-VGES! STORCH  *
      * APA01101!27.02.12!2.02 ! VGES DURCH MANDANT ERSETZT!STORCH   *
      * APA01932!16.07.15!2.03 ! PG-UST-BETRAG NEU         ! WANG    *
      * APA02280!20.06.16!2.04 ! PI-BEREICH UM PRODUKT ERW.! STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1039                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1039.
          05 PI-DRPA1039-PROGRAMM            PIC  X(008).
          05 PI-DRPA1039-UFKT                PIC  9(002).
          05 PI-DRPA1039-DATEN.
             10 PI-DRPA1039-PGEB-GES         PIC S9(04).
             10 PI-DRPA1039-VSTAT-KLASSE     PIC  X(01).
             10 PI-DRPA1039-ABRECH-DATUM-VON PIC  X(10).
             10 PI-DRPA1039-ABRECH-DATUM-BIS PIC  X(10).
             10 PI-DRPA1039-MANDANT          PIC  9(02).
             10 PI-DRPA1039-S-MANDANT        PIC  X(01).
200616       10 PI-DRPA1039-PRODUKT          PIC  9(03).
200616       10 PI-DRPA1039-S-PRODUKT        PIC  X(01).
      *
       01 PO-DRPA1039.
          05 PO-DRPA1039-RC                  PIC  X(003).
             88 PO-DRPA1039-RC-OK               VALUE 'OK '.
             88 PO-DRPA1039-RC-LEER             VALUE 'LER'.
             88 PO-DRPA1039-RC-EOF              VALUE 'EOF'.
      *
          05 PO-DRPA1039-DATEN.
             10 PO-DRPA1039-VSTAT            PIC  X(04).
             10 PO-DRPA1039-ANZ-BEWEGUNGEN   PIC  9(09) USAGE COMP.
             10 PO-DRPA1039-GSMT-PROV        PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
             10 PO-DRPA1039-ABRECH-DATUM     PIC  X(10).
160715       10 PO-DRPA1039-PG-UST-BETRAG    PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
      ******* ENDE COPY-MEMBER **************************************


