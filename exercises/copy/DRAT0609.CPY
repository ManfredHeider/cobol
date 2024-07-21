      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0609.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0609.CPv  $
      *
      *   Rev 2.2   Jun 25 2020 11:32:26   t13ae35
      *ARW00148: Anpassung der Umsatzsteuersätze - VB Leistung DRAT
      *0609 (jhe)
      *
      *   Rev 2.1   May 04 2016 17:23:00   t13ae35
      *AVB01400: Projekt Lucy2 - Konstanten-Copy DRAT0044
      *
      *   Rev 2.0   May 04 2016 17:14:30   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0609  KONSTANTEN FÜR KTY 402             *
      *                           USt. Typ                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    04.05.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!04.05.16!2.1 ! NEU                       ! J.Hering *
      * VLA-776 !25.06.20!2.2 ! TYP=3 hinzu               ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
      *----Regelsteuersatz Unternehmer -----------------------*
           05 K-KNR-UST-TYP-REGEL         PIC S9(004) BINARY VALUE  1.
      *----ermaessigter Steuersatz ---------------------------*
           05 K-KNR-UST-TYP-ERM           PIC S9(004) BINARY VALUE  2.
      *----Steuerschlüssel KU           -----------------------*
           05 K-KNR-UST-TYP-KU            PIC S9(004) BINARY VALUE  3.
      *
 


