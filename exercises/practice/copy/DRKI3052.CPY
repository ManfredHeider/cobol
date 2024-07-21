      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3052.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3052.CPv  $
      *
      *   Rev 2.1   Feb 22 2016 11:23:54   t13ae31
      *AKI01405: COC G50 CKV + G10 AMV Copie
      *
      *   Rev 2.0   Dec 18 2015 11:42:22   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01405!11.12.15!2.1 ! neu                       ! P.Hübner *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   DTA-Copy fuer COC Daten                                    *
      *                                                              *
      ****************************************************************
      *
      *01 COC-DTA
          02 'C'-VORSATZ.
             05 'C'-VS-SATZART PIC X(1).
 *********         'A'
             05 'C'-ABSENDER PIC X(3).
 *********   'AMV' 'AML' 'CKV' 'ADV'
             05 'C'-VS-DATUM   PIC 9(8).
 *********      Form JJJJMMDD
             05 FILLER PIC X(13).
 
          02 'C'-DATENSATZ REDEFINES 'C'-VORSATZ.
             05 'C'-DS-SATZART PIC X(1).
 *********         'D'
             05 'C'-VNR     PIC 9(15).
             05 'C'-DS-DATUM   PIC 9(8).
 *********      Form JJJMMDD
             05 'C'-AKTION PIC X(01).
 *********       S = SPERRUNG E = ENTSPERRUNG L = LÖSCHUNG
 
          02 'C'-NACHSATZ REDEFINES 'C'-VORSATZ.
             05 'C'-NS-SATZART PIC X(1).
 *********         'E'
             05 'C'-SATZANZ PIC 9(8).
 *********       ANZAHL DER DATENSÄTZE OHNE VOR- NACHSATZ
             05 FILLER PIC X(16).
      *
      ******* ENDE COPY-MEMBER DRKI3052 *****************************

