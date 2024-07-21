      * P13SP99   Stand: 06/12/98    18:02:21
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0628.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0628.CPv  $
      *
      *   Rev 2.2   Feb 24 2003 08:55:30   t13ex27
      *AKI00303 - Kommisarische Kundenübertragung
      *
      *   Rev 2.1   Nov 15 2002 15:37:02   t13ex58
      *AKI00305: Vertragsid eingefügt
      *
      *   Rev 2.0   Apr 16 1999 13:21:34   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:25:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0628  VERTRAGSDATEN FUER                 *
      *                           DIREKTIONSAENDERUNGEN ERMITTELN    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    19.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!19.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * AKI00305!21.10.02! 02 ! Feld VERTRAGSID eingef.   !W.MORFELD *
      * AKI00303!28.11.02! 03 ! Feld VRBKOMMBETRKZ eingef.!W.MORFELD *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRKI0628.
          05 PI-DRKI0628-KEY.
             10 PI-DRKI0628-UFKT               PIC  X(002).
          05 PI-DRKI0628-DATEN.
             10 PI-DRKI0628-DIRAENDPERSONENID  PIC  X(013).
             10 PI-DRKI0628-DIRAENDPERSROLLE   PIC  X(001).
      *
      ***    WIRD FUER DAS UPDATE: DTAERLEDIGT BENOETIGT
             10 PI-DRKI0628-DIRAENDID          PIC  X(013).


      *
       01 PO-DRKI0628.
          05 PO-DRKI0628-RC                    PIC  X(003).
             88 PO-DRKI0628-RC-OK                 VALUE 'OK '.
             88 PO-DRKI0628-RC-LEER               VALUE 'LER'.
             88 PO-DRKI0628-RC-EOF                VALUE 'EOF'.
             88 PO-DRKI0628-RC-FP                 VALUE 'FP '.
          05 PO-DRKI0628-DATEN.
             10 PO-DRKI0628-vertragsnr         pic  9(015).
             10 PO-DRKI0628-PARTGESELLSCHID    PIC  9(004).
             10 PO-DRKI0628-INTVERTRAGSNR      PIC  9(007).
             10 PO-DRKI0628-PRUEFZIFFER        PIC  9(002).
021021       10 PO-DRKI0628-VERTRAGSID         PIC  X(013).
021128       10 PO-DRKI0628-VRBKOMMBETRKZ      PIC  X(001).

      *
      ****** Ende Copy-Member ****************************************

