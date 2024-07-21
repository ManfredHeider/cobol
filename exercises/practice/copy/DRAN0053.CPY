      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0053.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAN0053.CPv  $
      *
      *   Rev 2.9   Nov 26 2009 14:33:26   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.8   Jun 19 2009 09:15:58   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.7   Mar 24 2009 10:56:28   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.6   Mar 11 2009 16:12:20   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.5   May 28 2008 08:49:56   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.4   01 Sep 2006 09:44:26   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.3   Jun 07 2002 10:20:12   t13ae34
      *AAN00357 - POA-AMV; mit Nettobeitrag
      *
      *   Rev 2.2   May 08 2002 16:07:08   t13ae34
      *AAN00357 - POA-AMV
      *
      *   Rev 2.1   Apr 23 2002 17:00:00   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0053                                     *
      *    FUNKTION   : ERM. POA-BESTAND ÜBER GES, VORSCHLAGSNUMMER  *
      *                 -> AUFBEREITET FÜR AMV/AN03 (BAN0018)        *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1  !NEU                        !HEIDER   *
      * AAN00357!06.06.02!2.2  !NETTOBEITRAG               !HEIDER   *
      * AAN00801!26.05.08!2.5  !KENNZEICHEN WIDERSPRUCH    !HEIDER   *
      * AAN00843!11.03.09! 2.7 !VERGRÖßERN XGDV-FELD POA   !RAHNER   *
      * AAN00884!17.06.09!2.8  !TARIFÄNDERUNG KFZ          !RAHNER   *
      * AAN00896!23.10.09!2.9  !OPTIMA                     !HEIDER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0053                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0053.
           05 PI-DRAN0053-POAA-KEY.
               10 PI-DRAN0053-PGEB-GES        PIC  X(2).
               10 PI-DRAN0053-POA-HVANTNUM    PIC  X(9).
      *           OPTIONAL
               10 PI-DRAN0053-POA-VDABVERM    PIC  X(7).
               10 PI-DRAN0053-POA-VDABVERMPZ  PIC  X(1).
      *
       01 PO-DRAN0053.
          05 PO-DRAN0053-RC                      PIC  X(003).
             88 PO-DRAN0053-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0053-RC-NOK                    VALUE 'NOK'.
             88 PO-DRAN0053-RC-KEY-NOT-UNIQUE         VALUE 'KNU'.
          05 PO-DRAN0053-DATEN.
             10 PO-DRAN0053-SONSTIGE.
                15 PO-DRAN0053-PIN-TAB OCCURS 5
                                       INDEXED BY I-DRAN0053-PIN,
                                                  I-DRAN0053-PIN-2.
                   20 PO-DRAN0053-POA-PIN       PIC S9(9) COMP.
                   20 PO-DRAN0053-POA-PIN-PZ    PIC  X(2).


             10 PO-DRAN0053-BAN0018O.

                15 PO-DRAN0053-PROD-TAB     OCCURS 5
                                       INDEXED BY I-DRAN0053-PROD,
                                                  I-DRAN0053-PROD-2.
      **           LFD. NR. BÜNDEL
                   20 PO-DRAN0053-HVANTNUM-LFD  PIC X(1).
                   20 PO-DRAN0053-PROD2O        PIC X(2).
                   20 PO-DRAN0053-BEGIN2O       PIC X(6).
                   20 PO-DRAN0053-ABLAU2O       PIC X(6).
                   20 PO-DRAN0053-ZAHLW2O       PIC X(1).
                   20 PO-DRAN0053-BEIT2O        PIC X(8).
                   20 PO-DRAN0053-VDABLEIN      PIC S9(6)V9(1) COMP-3.
                   20 PO-DRAN0053-BEITRAGNE     PIC S9(9)V9(2) COMP-3.


                15 PO-DRAN0053-REST.
                   20 PO-DRAN0053-VB21O          PIC X(7).
                   20 PO-DRAN0053-PRZVB21O       PIC X(1).
                   20 PO-DRAN0053-ZNAME21O       PIC X(26).
                   20 PO-DRAN0053-VNAME21O       PIC X(26).
                   20 PO-DRAN0053-PLZ21O         PIC X(5).
                   20 PO-DRAN0053-ORT21O         PIC X(21).
                   20 PO-DRAN0053-AUSL21O        PIC X(3).

                   20 PO-DRAN0053-HAND21O        PIC X(1).
                   20 PO-DRAN0053-ANART21O       PIC X(1).
                   20 PO-DRAN0053-WKZ21O         PIC X(1).
                   20 PO-DRAN0053-WKZT21O        PIC X(3).
                   20 PO-DRAN0053-ANDAT21O       PIC X(6).
                   20 PO-DRAN0053-ERFD21O        PIC X(6).
                   20 PO-DRAN0053-ASTTE21O       PIC X(6).
                   20 PO-DRAN0053-STAT21O        PIC X(2).

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0056.


      ******** ENDE COPY-MEMBER **************************************
