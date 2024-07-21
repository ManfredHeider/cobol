      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2395.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2395.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:32:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Stornoneutrale Aufh.LV-Verträge/Eigen (Regionen)
      *
      *     SOURCE-NAME      :  DRMA2395
      *     DMR-NAME         :  DR-OV2395
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  10.05.01/10:25
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA2395.
000020        07 VBNR                   PICTURE X(10).
000030        07 STRUK-DIR              PICTURE 999.
000040        07 PROVEINH               OCCURS 13 TIMES
000050                                  INDEXED BY IND
000060                                  PICTURE S9(9)V9
000070                                                 PACKED-DECIMAL.
000080        07 KNR-REGION             PICTURE 9(8).
000090        07 KNR-REGION-BEZ         PICTURE X(20).
      *
      **************************************************************
