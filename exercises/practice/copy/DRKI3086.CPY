      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3086.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3086.CPv  $
      *
      *   Rev 2.1   Sep 12 2016 11:04:26   t13ex78
      *AKI01463: Programme, Copies ect.
      *
      *   Rev 2.0   Aug 25 2016 08:08:02   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *  Copy Member für die Aufbereitung der CKV-Daten                *
      *  Hier werden die Fremdversichererdaten abgelegt welche im Nach-*
      *  gang mit dem Programm PMKI1306 in die KRANKENPERSEIGEN ge-    *
      *  bracht werden.                                                *
      *                                                                *
      *  Satzlaenge    80                                              *
      *                                                                *
      *                                                                *
      ******************************************************************
      *****************************************************************
      * LOGBUCH
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01463!25.08.16!2.0 ! Neuerstellung wegen Ein-  ! tfl      *
      *         !        !    ! fuehrung Fremdversicherer-!          *
      *         !        !    ! daten und Korrespondenz-  !          *
      *         !        !    ! makler                    !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
         01 DRKI3086.
      * nur Update (U)
           05  'XX'-AKTUAL-KZ              PIC X(01).
           05  'XX'-GESELLSCHAFT           PIC 9(02).
           05  'XX'-VERTRAGSNUMMER         PIC 9(15).
      * laufende Nummer der vers. Person
           05  'XX'-LFDNR                  PIC 9(02).
      * Gesellschaft, bei der die Fremdversicherung laeuft.
      * Mapping ueber TBAT0039 mit KTY 4465
           05  'XX'-VORVERSSCHL            PIC 9(03).
      * Datum zu dem die Fremdversicherung endet
           05  'XX'-VORVERSDATUM           PIC 9(08).
      * Kuendigung der Fremdversicherung liegt vor? J oder N
           05  'XX'-VORVERSKUENDKZ         PIC X(01).
 
           05  FILLER                      PIC X(48).
 
 
 
 

