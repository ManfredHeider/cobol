      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0054.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0054.CPv  $
      *
      *   Rev 2.8   Oct 24 2013 11:00:12   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.7   Jun 19 2009 09:16:04   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.6   Mar 24 2009 10:56:34   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.5   Mar 11 2009 16:12:24   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.4   May 28 2008 08:50:00   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.3   23 Nov 2007 10:17:06   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.2   01 Sep 2006 09:44:36   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Apr 23 2002 17:00:02   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0054                                     *
      *    FUNKTION   : ERM. POA-BESTAND ÜBER GES, PIN               *
      *                 -> AUFBEREITET FÜR AMV/AN03 (BAN0018)          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1  !NEU                        !HEIDER   *
      * AAN00730!30.11.07!2.3  !ALLFINANZ                  !RAHNER   *
      * AAN00801!26.05.08!2.4  !KENNZEICHEN WIDERSPRUCH    !HEIDER   *
      * AAN00843!11.03.09! 2.6 !VERGRÖßERN XGDV-FELD POA   !RAHNER   *
      * AAN00884!17.06.09!2.7  !TARIFÄNDERUNG KFZ          !RAHNER   *
      * AAN01073!24.10.13!2.8  !SEPA                       !RAHNER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0054                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0054.
           05 PI-DRAN0054-POAA-KEY.
               10 PI-DRAN0054-PGEB-GES        PIC  X(2).
               10 PI-DRAN0054-POA-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0054-POA-VDABVERM    PIC  S9(9) COMP.
      *
       01 PO-DRAN0054.
          05 PO-DRAN0054-RC                      PIC  X(003).
             88 PO-DRAN0054-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0054-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0054-DATEN.
      *  OUTPUT DATA FOR MAP BAN0018
             10 PO-DRAN0054-BAN0018.
                15 PO-DRAN0054-ZNAME21O       PIC X(26).
                15 PO-DRAN0054-VNAME21O       PIC X(26).
                15 PO-DRAN0054-PLZ21O         PIC X(5).
                15 PO-DRAN0054-ORT21O         PIC X(21).
                15 PO-DRAN0054-AUSL21O        PIC X(3).
                15 PO-DRAN0054-PROD21O        PIC X(2).
                15 PO-DRAN0054-PROD22O        PIC X(2).
                15 PO-DRAN0054-PROD23O        PIC X(2).
                15 PO-DRAN0054-PROD24O        PIC X(2).
                15 PO-DRAN0054-PROD25O        PIC X(2).
                15 PO-DRAN0054-BEGIN21O       PIC X(6).
                15 PO-DRAN0054-BEGIN22O       PIC X(6).
                15 PO-DRAN0054-BEGIN23O       PIC X(6).
                15 PO-DRAN0054-BEGIN24O       PIC X(6).
                15 PO-DRAN0054-BEGIN25O       PIC X(6).
                15 PO-DRAN0054-ABLAU21O       PIC X(6).
                15 PO-DRAN0054-ABLAU22O       PIC X(6).
                15 PO-DRAN0054-ABLAU23O       PIC X(6).
                15 PO-DRAN0054-ABLAU24O       PIC X(6).
                15 PO-DRAN0054-ABLAU25O       PIC X(6).
                15 PO-DRAN0054-ZAHLW21O       PIC X(1).
                15 PO-DRAN0054-ZAHLW22O       PIC X(1).
                15 PO-DRAN0054-ZAHLW23O       PIC X(1).
                15 PO-DRAN0054-ZAHLW24O       PIC X(1).
                15 PO-DRAN0054-ZAHLW25O       PIC X(1).
                15 PO-DRAN0054-BEIT21O        PIC X(8).
                15 PO-DRAN0054-BEIT22O        PIC X(8).
                15 PO-DRAN0054-BEIT23O        PIC X(8).
                15 PO-DRAN0054-BEIT24O        PIC X(8).
                15 PO-DRAN0054-BEIT25O        PIC X(8).
                15 PO-DRAN0054-ANART21O       PIC X(1).
                15 PO-DRAN0054-WKZ21O         PIC X(1).
                15 PO-DRAN0054-WKZT21O        PIC X(3).
                15 PO-DRAN0054-ANDAT21O       PIC X(6).
                15 PO-DRAN0054-ERFD21O        PIC X(6).
                15 PO-DRAN0054-ASTTE21O       PIC X(6).
                15 PO-DRAN0054-STAT21O        PIC X(2).

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0056.


      ******** ENDE COPY-MEMBER **************************************
