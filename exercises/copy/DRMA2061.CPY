      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2061.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2061.CPv  $
      *
      *   Rev 2.3   Jun 12 2018 09:42:52   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.2   Sep 26 2014 13:39:34   t13ac19
      *AMA00079: Projekt Integration FVD: techn. Konzeption "VB-Abr
      *echnung": Copies
      *
      *   Rev 2.1   Jul 01 2013 11:28:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!01.07.13!2.1  ! ÜBERNAHME AUS MIGRATION   ! O.WILHELM
      * AMA00079!26.09.14!2.2  ! Einträge für VG 1304      ! O.WILHELM
      *         !        !     ! gelöscht                  !
      * AMA00400!26.02.18!2.3  ! Einträge für VG 1305      ! D.WILHELM
      *         !        !     ! (Allfinanz AG) eingefügt  !
      *         !        !     ! Dummy-Einträge wg. Verarb.!
      *         !        !     ! in UPMA2518               !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *---------------------------------------------------------------*
      * Kurzbeschreibung:
      * ================
      *    Die Copy-Strecke enthält die Referenztabelle für
      *    relevante Informationen bei File-Transfers zur DVAG
      *    (SST- und Job-Name).
      *
      *
      * Voraussetzungen:
      * ===============
      *    Es gibt keine besonderen Voraussetzungen.
      *
      *
      * Funktionale Beschreibung:
      * ========================
      *    Die Werte sind in einer zweistufigen Tabelle hinterlegt.
      *    Die erste Stufe bildet die VG, wobei der Tabellenplatz
      *    den Stellen 3 und 4 der VG entspricht. Die zweite Stufe
      *    bildet die File-Transfernummer.
      *
      *---------------------------------------------------------------*
       01  TABELLE-FTPINF.
           05 TABFI-WERTE.
              10 TABM-WERT-1301-001 pic X(17) value 'STVD0111|VD004105'.
              10 TABM-WERT-1301-002 pic X(17) value 'STVD0115|VD004105'.
              10 TABM-WERT-1302-001 pic X(17) value 'STVD0111|VD004105'.
              10 TABM-WERT-1302-002 pic X(17) value 'STVD0115|VD004105'.
              10 TABM-WERT-1303-001 pic X(17) value 'STVD0111|VD004105'.
              10 TABM-WERT-1303-002 pic X(17) value 'STVD0115|VD004105'.
              10 TABM-WERT-0000-001 pic X(17) value 'NICHT   |GENUTZT '.
              10 TABM-WERT-0000-002 pic X(17) value 'NICHT   |GENUTZT '.
              10 TABM-WERT-1305-001 pic X(17) value 'STVD0111|VD004105'.
              10 TABM-WERT-1305-002 pic X(17) value 'STVD0115|VD004105'.
           05 redefines TABFI-WERTE.
              10 TABFI.
                 15                      occurs 5
                                         indexed by TABFI-IND1.
                    20                   occurs 2
                                         indexed by TABFI-IND2.
                       25 TABFI-SST-NAME pic X(8).
                       25                pic X.
                       25 TABFI-JOBNAME  pic X(8).

