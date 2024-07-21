      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0013.CPv  $
      *
      *   Rev 2.1   Apr 05 2004 11:33:30   t13ex22
      *AAS00454: Schnittstellenprogramm für Startseite VB-
      *Portal
      *
      *   Rev 2.0   06 Mar 2001 11:47:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      *    COPY-MEMBER: SCHNITTSTELLE FÜR VB-PORTAL
      ***************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AAS00454!06.02.04!2.0 ! NEU                       ! MUND
      ***************************************************************
           03 SST.
               05 VB-NR                 PIC 9(007).
               05 STUFE-AKT             PIC 9(004).
               05 STUFE-VORHER          PIC 9(004).
               05 STUFE-BEFOERD-DATUM   PIC X(010).
               05 STUFE-NEU             PIC 9(004).
      *        05 EH-EIG-ALT            PIC 9(009)V9.
               05 EH-EIG-ALT            PIC -9(08)V9.
               05 EQF-EIG-ALT           PIC 9(003)V99.
      *        05 EH-GRU-ALT            PIC 9(009)V9.
               05 EH-GRU-ALT            PIC -9(08)V9.
               05 EQF-GRU-ALT           PIC 9(003)V99.
      *        05 EH-GES-ALT            PIC 9(009)V9.
               05 EH-GES-ALT            PIC -9(08)V9.
               05 EQF-GES-ALT           PIC 9(003)V99.
      *        05 EH-EIG-NEU            PIC 9(009)V9.
               05 EH-EIG-NEU            PIC -9(08)V9.
               05 EQF-EIG-NEU           PIC 9(003)V99.
      *        05 EH-GRU-NEU            PIC 9(009)V9.
               05 EH-GRU-NEU            PIC -9(08)V9.
               05 EQF-GRU-NEU           PIC 9(003)V99.
      *        05 EH-GES-NEU            PIC 9(009)V9.
               05 EH-GES-NEU            PIC -9(08)V9.
               05 EQF-GES-NEU           PIC 9(003)V99.
               05 ANZ-PART-NEU          PIC 9(003).
               05 ANZ-PART-HPTB-NEU     PIC 9(003).
               05 ALP-ANZ-VM-ODER-NEU   PIC 9(003).
               05 PRAXIS-AKT            PIC 9(004).
               05 PRAXIS-VORHER         PIC 9(004).
               05 PRAXIS-BEFOERD-DATUM  PIC X(010).
               05 PRAXIS-NEU            PIC 9(004).
      *        05 EH-EIG-VBP            PIC 9(009)V9.
               05 EH-EIG-VBP            PIC -9(08)V9.
      *        05 EH-EIG-WEIT-VBP       PIC 9(009)V9.
               05 EH-EIG-WEIT-VBP       PIC -9(08)V9.
               05 EQF-EIG-VBP           PIC 9(003)V99.
               05 ANZ-PART-ALT          PIC 9(003).
      ********************* ENDE COPY DRAS0013 *************************
