      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0934.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0934.CPv  $
      *
      *   Rev 2.3   Sep 09 2015 16:29:26   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11, PO22)
      *
      *   Rev 2.2   19 May 2006 10:40:54   t13ae46
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.1   Mar 10 2003 09:27:14   t13ex21
      *FPA00540 - Erweiterung um Dynamik-KZ
      *
      *   Rev 2.0   May 26 1999 15:37:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0934 DATENFELDER FüR DIALOG PO22         *
      *                          'LIST PM JE PRODUKT'                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    05.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!05.08.98! 01 !NEU                        !BOHLINGER *
      * FPA00540!14.02.03!02.1! ERWEITERUNG DYNAMIK-KZ    ! STORCH   *
      * FPA00846!15.05.06!2.2 ! ERWEITERUNG SONDERHAFT-KZ ! WANG     *
      * APA01932!25.08.15!2.3 ! PG-UST-BETRAG NEU         ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FüR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG PO22                      *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FüR LISTDIALOG PO22
            35 PMBE-RFOLGE-NR                   PIC S9(004) BINARY.
            35 PMBE-PGFAELLIG-VON               PIC  X(010).
            35 PMBE-PGFAELLIG-BIS               PIC  X(010).
            35 PMPR-ID                          PIC S9(015)
                                                PACKED-DECIMAL.
            35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
            35 PROD-SCHL-BM                     PIC  X(010).
            35 PMPR-VSTAT                       PIC  X(004).
            35 PMPR-PGV                         PIC S9(004) BINARY.
            35 PMPR-PG-GSMT-PROV                PIC S9(009)V99
                                                PACKED-DECIMAL.
            35 KNR-WAEHRUNG-BK                  PIC  X(004).
140203      35 DYNAMIK-KZ                       PIC  X(001).
150506      35 SONDERHAFT-KZ                    PIC  X(002).
250815      35 PG-UST-BETRAG                    PIC S9(9)V9(2) COMP-3.



