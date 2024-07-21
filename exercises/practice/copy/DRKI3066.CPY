      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3066.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3066.CPv  $
      *
      *   Rev 2.1   Jun 20 2016 07:43:58   t13ae31
      *AKI01424: G10 SUH Bestandsmanagement Pgme
      *
      *   Rev 2.0   Mar 24 2016 15:13:22   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01424!24.03.16!2.1 ! neu                       ! P.Hübner *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   Copy fuer BMG Daten VERARBEITUNG                           *
      *                                                              *
      ****************************************************************
      *
      *01 BMG-DTA
          02 'C'-KEY.
             05 'C'-VNR        PIC 9(15).
          02 'C'-DATENSATZ.
             05 'C'-KZ     PIC X(01).
 *********       G = generell umstellungsfähig
 *********       N = generell nicht umstellungsfähig
 *********       T = teilweise umstellungsfähig
             05 'C'-BESCHREIBUNG PIC X(30).
             05 FILLER PIC X(04).
 
      ******* ENDE COPY-MEMBER DRKI3066 *****************************
 

