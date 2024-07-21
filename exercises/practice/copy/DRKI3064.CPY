      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3064.CPv  $
      *
      *   Rev 2.1   Jun 20 2016 07:43:52   t13ae31
      *AKI01424: G10 SUH Bestandsmanagement Pgme
      *
      *   Rev 2.0   Mar 16 2016 12:24:40   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01424!16.03.16!2.1 ! neu                       ! P.Hübner *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   DTA-Copy fuer BMG Daten                                    *
      *                                                              *
      ****************************************************************
      *
      *01 BMG-DTA
          02 'C'-VORSATZ.
             05 'C'-VS-SATZART PIC X(1).
 *********         'A'
             05 'C'-ABSENDER PIC X(3).
 *********   'AMV'
             05 'C'-VS-DATUM   PIC 9(8).
 *********      Form JJJJMMDD
             05 FILLER PIC X(38).
 
          02 'C'-DATENSATZ REDEFINES 'C'-VORSATZ.
             05 'C'-DS-SATZART PIC X(1).
 *********         'C'
             05 'C'-VNR     PIC 9(15).
             05 'C'-KZ     PIC X(01).
 *********       G = generell umstellungsfähig
 *********       N = generell nicht umstellungsfähig
 *********       T = teilweise umstellungsfähig
             05 'C'-BESCHREIBUNG PIC X(30).
             05 FILLER PIC X(03).
 
          02 'C'-NACHSATZ REDEFINES 'C'-VORSATZ.
             05 'C'-NS-SATZART PIC X(1).
 *********         'E'
             05 'C'-SATZANZ PIC 9(8).
 *********       ANZAHL DER DATENSÄTZE OHNE VOR- NACHSATZ
             05 FILLER PIC X(41).
      *
      ******* ENDE COPY-MEMBER DRKI3064 *****************************
 

