      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0224.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0224.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 09:11:24   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.2   Nov 02 2016 09:34:58   t13ae34
      *FAN01098: Badenia - Doppeltlieferung maschinelle Anträge
      *
      *   Rev 2.1   Apr 23 2009 10:16:38   t13ae34
      *AAN00882: Antrag
      *
      *   Rev 2.0   Apr 08 2009 11:43:38   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  DATEN FÜR SETZEN ANTRAGSTATUS
      ****************************************************************
      *    COPY-MEMBER: DRAN0224                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00882!21.04.09!2.1 !NEU                        ! HEIDER   *
      * FAN01098!31.10.16!2.2 !DEL                        ! HEIDER   *
      * AAN01351!13.03.18!2.3 !DSG                        ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  DRAN0224-DATEN.

      ****     VERARBEITUNGSKENNZEICHEN.
      ****     WERTEBEREICH: OK  -> UPDATE
      ****                   NFD -> KEIN UPDATE, ANTRAG NICHT GEFUNDEN
      ****                   NUP -> KEIN UPDATE, AUS FACHLICHEN GRÜNDEN
      ****                   DEL -> LÖSCHEN ANTRAG
      ****                   DSG -> LÖSCHEN ANTRAG IM RAHMEN DSGVO
               10 DRAN0224-VERARB-KENNZ                 PIC X(03).
      ****     PRODUKTGEBERGESELLSCHAFT
               10 DRAN0224-PGEB-GES                     PIC 9(04).
      ****     ANTRAGSNUMMER
               10 DRAN0224-NR                           PIC 9(15).
      ****     ANTRAGSTATUS
               10 DRAN0224-STATUS                       PIC 9(04).
      ****     FILLER
               10 DRAN0224-FILLER                       PIC X(04).
      *
      ******** ENDE COPY-MEMBER **************************************

