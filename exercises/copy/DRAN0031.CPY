      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0031.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0031.CPv  $
      *
      *   Rev 2.8   27 Dec 2007 11:10:40   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.7   08 Sep 2006 09:40:12   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.6   Oct 28 2004 14:24:56   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.5   Jul 02 2004 14:09:40   t13ae34
      *AAN00502: Dialogverarbeitung: AN50 UU5n
      *
      *   Rev 2.4   Dec 18 2001 14:40:24   t13ex33
      *AAT00022 - Euro-Umstellung
      *
      *   Rev 2.3   13 Sep 2001 17:02:48   t13ex33
      *AAN00320 - EURO-Umstellung - euro-vorbereitet
      *
      *   Rev 2.2   03 May 2001 12:52:18   t13ae34
      *FAN00304-doppelte PIN bei PZM
      *
      *   Rev 2.1   27 Feb 2001 11:49:58   T13AE34
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
      *    COPY-MEMBER: DRAN0031                                     *
      *    FUNKTION   : ERM. PZM-BESTAND ÜBER GES, PIN               *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.12.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00268!27.11.00!2.1 !NEU                         !HEIDER   *
      * FAN00300!25.04.01!2.2 !NEU                         !HEIDER   *
EURO*** AAN00320!15.08.01!    ! EURO-UMSTELLUNG            ! ANIKEEV *
      *         !        !    ! MAP BAN0014 NEU AUS UU917  !         *
      * AAN00531!22.10.04!2.6 !AML NEUE TARIFE NACH AEG    !HEIDER   *
      * AAN00685!08.09.06!2.7 !UMSTELLUNG AM-LEBEN AUF     !         *
      *         !        !    !BLACK-BOX-VERFAHREN         !RAHNER   *
      * AAN00730!28.12.07!2.8 !ALLFINANZ                   !RAHNER   *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0031                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0031.
           05 PI-DRAN0031-PZMA-KEY.
               10 PI-DRAN0031-PGEB-GES        PIC  X(2).
               10 PI-DRAN0031-PZM-PIN         PIC  S9(9) COMP.
030501         10 PI-DRAN0031-PZM-VDABVERM    PIC  S9(9) COMP.
      *
       01 PO-DRAN0031.
          05 PO-DRAN0031-RC                      PIC  X(003).
             88 PO-DRAN0031-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0031-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0031-DATEN.
      *  OUTPUT DATA FOR MAP BAN0014 (alt:UU917)
 EURO-*      10 PO-DRAN0031-UU917O.
 EURO+       10 PO-DRAN0031-BAN0014O.
                15 PO-DRAN0031-VBNR1O       PIC X(7).
                15 PO-DRAN0031-PRZVB1O      PIC X(1).
                15 PO-DRAN0031-ZNAME1O      PIC X(26).
                15 PO-DRAN0031-VNAME1O      PIC X(26).
                15 PO-DRAN0031-PLZ1O        PIC X(5).
                15 PO-DRAN0031-ORT1O        PIC X(21).
                15 PO-DRAN0031-AUSL1O       PIC X(3).
                15 PO-DRAN0031-GEBDAT1O     PIC X(4).
                15 PO-DRAN0031-TARIF1O      PIC X(10).
                15 PO-DRAN0031-SUMME1O      PIC X(9).
                15 PO-DRAN0031-GEBDA21O     PIC X(4).
                15 PO-DRAN0031-BEGINN1O     PIC X(6).
010704*         15 PO-DRAN0031-DAUHV1O      PIC X(2).
010704          15 PO-DRAN0031-DAUHV1O      PIC X(6).
010704*         15 PO-DRAN0031-DBEIZ1O      PIC X(2).
010704          15 PO-DRAN0031-DBEIZ1O      PIC X(6).
221004*         15 PO-DRAN0031-DBUZ1O       PIC X(2).
221004*         15 PO-DRAN0031-REN1O        PIC X(3).
221004*         15 PO-DRAN0031-RDM1O        PIC X(6).
221004*         15 PO-DRAN0031-DPROZ1O      PIC X(2).
                15 PO-DRAN0031-BEITR1O      PIC X(9).
                15 PO-DRAN0031-ZW1O         PIC X(1).
221004*         15 PO-DRAN0031-VZAN1O       PIC X(1).
221004          15 PO-DRAN0031-DIVE1O       PIC X(1).
221004*         15 PO-DRAN0031-BAV1O        PIC X(1).
221004          15 PO-DRAN0031-TS1O         PIC X(1).
                15 PO-DRAN0031-SCHBET1O     PIC X(8).
                15 PO-DRAN0031-ANART1O      PIC X(1).
                15 PO-DRAN0031-WKZ1O        PIC X(1).
                15 PO-DRAN0031-WKZT1O       PIC X(3).

      *    PC-DATEN OHNE AUFBEREITUNG
080906*    COPY DRAN0043.
080906     COPY DRAN0113.


      ******** ENDE COPY-MEMBER **************************************
