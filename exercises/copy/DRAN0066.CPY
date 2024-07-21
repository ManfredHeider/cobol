      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0066.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0066.CPv  $
      *
      *   Rev 2.3   27 Dec 2007 11:38:34   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.2   01 Sep 2006 09:45:50   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:22   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:15:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0066                                     *
      *    FUNKTION   : ERM. VIA-BESTAND ÜBER GES, VORSCHLAGSNUMMER  *
      *                 -> AUFBEREITET FÜR BADENIA                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1  !NEU                        !RAHNER   *
      * AAN00730!28.12.07!2.3  !ALLFINANZ                  !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0066                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0066.
           05 PI-DRAN0066-VIA-KEY.
               10 PI-DRAN0066-PGEB-GES           PIC  X(2).
               10 PI-DRAN0066-VIA-HVANTNUM       PIC S9(17) COMP-3.
      *           OPTIONAL
               10 PI-DRAN0066-VIA-VDABVERM       PIC  X(7).
               10 PI-DRAN0066-VIA-VDABVERMPZ     PIC  X(1).
      *
       01 PO-DRAN0066.
          05 PO-DRAN0066-RC                      PIC   X(003).
             88 PO-DRAN0066-RC-OK                VALUE 'OK '.
             88 PO-DRAN0066-RC-NOK               VALUE 'NOK'.
             88 PO-DRAN0066-RC-KEY-NOT-UNIQUE    VALUE 'KNU'.
          05 PO-DRAN0066-DATEN.
      *  OUTPUT DATA FOR MAP BAN0010
             10 PO-DRAN0066-VBNRA8O              PIC X(7).
             10 PO-DRAN0066-PRZVB8O              PIC X(1).
             10 PO-DRAN0066-ZNAME8O              PIC X(26).
             10 PO-DRAN0066-VNAME8O              PIC X(26).
             10 PO-DRAN0066-PLZA8O               PIC X(5).
             10 PO-DRAN0066-ORTA8O               PIC X(21).
             10 PO-DRAN0066-AUSA8O               PIC X(3).
             10 PO-DRAN0066-TARIF8O              PIC X(4).
             10 PO-DRAN0066-DVAG-PRODKEY         PIC 9(4).
             10 PO-DRAN0066-SUMME8O              PIC 9(9).
             10 PO-DRAN0066-VDABLEIN             PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0066-KENNZ-BSV            PIC X(1).
             10 PO-DRAN0066-KENNZ-TARIF          PIC X(1).
             10 PO-DRAN0066-KENNZ-FRST           PIC X(1).
             10 PO-DRAN0066-KENNZ-LEG            PIC X(1).
             10 PO-DRAN0066-KENNZ-VL             PIC X(1).
             10 PO-DRAN0066-KENNZ-VL2            PIC X(1).
             10 PO-DRAN0066-KENNZ-ERHOEH         PIC X(1).
             10 PO-DRAN0066-HAND-BSVO            PIC X(1).
             10 PO-DRAN0066-HAND-TARO            PIC X(1).
             10 PO-DRAN0066-HAND-FRSO            PIC X(1).
             10 PO-DRAN0066-HAND-LEGO            PIC X(1).
             10 PO-DRAN0066-HAND-VLO             PIC X(1).
             10 PO-DRAN0066-HAND-VL2O            PIC X(1).
             10 PO-DRAN0066-ANART8O              PIC X(1).
             10 PO-DRAN0066-ANDAT8O              PIC X(8).
             10 PO-DRAN0066-WKZ8O                PIC X(1).
             10 PO-DRAN0066-WKZT8O               PIC X(3).
             10 PO-DRAN0066-SONSTIGE.
                15 PO-DRAN0066-PIN8O             PIC S9(9) COMP.
                15 PO-DRAN0066-PIN28O            PIC S9(9) COMP.


      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0063.


      ******** ENDE COPY-MEMBER **************************************

