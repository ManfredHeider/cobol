      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0030.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRAN0030.CPv  $
      *
      *   Rev 2.11   27 Dec 2007 11:10:30   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.10   08 Sep 2006 09:40:00   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.9   19 May 2006 15:06:10   t13ae23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.8   10 May 2006 17:02:40   T13AE23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.7   Nov 14 2005 15:08:56   t13ae34
      *AAN00626: Einführung des Tarifes FRBZ
      *
      *   Rev 2.6   Oct 28 2004 09:04:56   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.5   Jul 02 2004 14:05:58   t13ae34
      *AAN00502: Dialogverarbeitung: AN50 UU5n
      *
      *   Rev 2.4   Dec 18 2001 14:40:16   t13ex33
      *AAT00022 - Euro-Umstellung
      *
      *   Rev 2.3   13 Sep 2001 17:02:40   t13ex33
      *AAN00320 - EURO-Umstellung - euro-vorbereitet
      *
      *   Rev 2.2   26 Apr 2001 17:34:20   t13ae34
      *FAN00300-PZM-Antrag: Anpassungen 
      *
      *   Rev 2.1   27 Feb 2001 11:49:52   T13AE34
      *AAN00268-PZM Antrag mit FAN00288
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0030                                     *
      *    FUNKTION   : ERM. PZM-BESTAND ÜBER GES, VORSCHLAGSNUMMER  *
      *                 -> AUFBEREITET FÜR AML/AN50 (BAN0014)        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    27.11.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00268!27.11.00!2.1 !NEU                         !HEIDER   *
      * FAN00300!25.04.01!2.2 !NEU                         !HEIDER   *
EURO*** AAN00320!15.08.01!    ! EURO-Umstellung            ! Anikeev *
      *         !        !    ! map BAN0014 neu aus UU917  !         *
      * AAN00531!22.10.04!2.6 !AML NEUE TARIFE NACH AEG    !HEIDER   *
      * AAN00626!25.10.05!2.7 !FRBZ -> HIER: TARIF 15 ST.  !HEIDER   *
      * AAN00666!10.05.06!2.8 !NEUE RÜRUP-TARIFE           !RAHNER   *
      * AAN00685!08.09.06!2.9 !UMSTELLUNG AM-L AUF BLACK-  !         *
      *         !        !    !BOX VERFAHREN               !RAHNER   *
      * AAN00730!28.12.07!2.11!ALLFINANZ                   !RAHNER   *
      *         !        !    !                            !         *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0030                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0030.
           05 PI-DRAN0030-PZMA-KEY.
               10 PI-DRAN0030-PGEB-GES        PIC  X(2).
               10 PI-DRAN0030-PZM-HVANTNUM    PIC  X(9).
      *           OPTIONAL
               10 PI-DRAN0030-PZM-VDABVERM    PIC  X(7).
               10 PI-DRAN0030-PZM-VDABVERMPZ  PIC  X(1).
      *
       01 PO-DRAN0030.
          05 PO-DRAN0030-RC                      PIC  X(003).
             88 PO-DRAN0030-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0030-RC-NOK                    VALUE 'NOK'.
             88 PO-DRAN0030-RC-KEY-NOT-UNIQUE         VALUE 'KNU'.
          05 PO-DRAN0030-DATEN.
      *  OUTPUT DATA FOR MAP BAN0014 (alt:UU917)
 EURO-*      10 PO-DRAN0030-UU917O.
 EURO+       10 PO-DRAN0030-BAN0014O.
                15 PO-DRAN0030-VBNR1O       PIC X(7).
                15 PO-DRAN0030-PRZVB1O      PIC X(1).
                15 PO-DRAN0030-ZNAME1O      PIC X(26).
                15 PO-DRAN0030-VNAME1O      PIC X(26).
                15 PO-DRAN0030-PLZ1O        PIC X(5).
                15 PO-DRAN0030-ORT1O        PIC X(21).
                15 PO-DRAN0030-AUSL1O       PIC X(3).
                15 PO-DRAN0030-GEBDAT1O     PIC X(4).
251005*         15 PO-DRAN0030-TARIF1O      PIC X(10).
251005          15 PO-DRAN0030-TARIF1O      PIC X(15).
                15 PO-DRAN0030-SUMME1O      PIC X(9).
                15 PO-DRAN0030-GEBDA21O     PIC X(4).
                15 PO-DRAN0030-BEGINN1O     PIC X(6).
010704*         15 PO-DRAN0030-DAUHV1O      PIC X(2).
010704          15 PO-DRAN0030-DAUHV1O      PIC X(6).
010704*         15 PO-DRAN0030-DBEIZ1O      PIC X(2).
010704          15 PO-DRAN0030-DBEIZ1O      PIC X(6).
221004*         15 PO-DRAN0030-DBUZ1O       PIC X(2).
221004*         15 PO-DRAN0030-REN1O        PIC X(3).
221004*         15 PO-DRAN0030-RDM1O        PIC X(6).
221004*         15 PO-DRAN0030-DPROZ1O      PIC X(2).
                15 PO-DRAN0030-BEITR1O      PIC X(9).
                15 PO-DRAN0030-ZW1O         PIC X(1).
221004*         15 PO-DRAN0030-VZAN1O       PIC X(1).
221004          15 PO-DRAN0030-DIVE1O       PIC X(1).
221004*         15 PO-DRAN0030-BAV1O        PIC X(1).
221004          15 PO-DRAN0030-TS1O         PIC X(1).
                15 PO-DRAN0030-SCHBET1O     PIC X(8).
                15 PO-DRAN0030-ANART1O      PIC X(1).
                15 PO-DRAN0030-WKZ1O        PIC X(1).
                15 PO-DRAN0030-WKZT1O       PIC X(3).

             10 PO-DRAN0030-SONSTIGE.
                15 PO-DRAN0030-PZM-PIN         PIC S9(9) COMP.

      *    PC-DATEN OHNE AUFBEREITUNG
080906*    COPY DRAN0043.
080906     COPY DRAN0113.


      ******** ENDE COPY-MEMBER **************************************
