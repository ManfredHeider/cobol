      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0408.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0408.CPv  $
      *
      *   Rev 2.5   Feb 21 2007 12:37:34   t13ae34
      *ACV00054: Change Request CTV-Service-Funktion
      *
      *   Rev 2.4   Mar 06 2006 10:10:38   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.3   Mar 06 2006 09:57:34   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.2   Feb 20 2006 11:59:44   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.1   Jan 27 2006 11:38:10   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.0   Nov 25 2005 14:32:32   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0408                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN DIALOGEN (CADD-I-KOMM-VAR, CADD-O-KOMM-VAR)       *
      *    FÜR ZA13 (CTV-SOFORTDRUCK)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    28.11.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00054!28.11.05!2.1 ! NEU                       ! HEIDER   *
      * ACV00054!30.01.06!2.2 ! SIB-ANHANG DURCHREICHEN   ! HEIDER   *
      * ACV00054!06.03.06!2.4 ! CTV-MELDUNG 8-STELLIG     ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRAT0408.
           05  PI-DRAT0408-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0408-CADD-I-KOMM-VAR.
      *
               10  PI-DRAT0408-FUNKTION               PIC  X(008).
      *                CTV-SOFORTDRUCK
                   88  PI-DRAT0408-CTV                VALUE 'DRAT0408'.

               10  PI-DRAT0408-VB-NR                  PIC  X(007).
               10  PI-DRAT0408-VB-NR-9 REDEFINES
                   PI-DRAT0408-VB-NR                  PIC  9(007).
      *            BRIEFBEZEICHNUNG
               10  PI-DRAT0408-SIB-REC-ID             PIC  X(010).
      *
       01  PO-DRAT0408.
      *    MELDUNG AUS CTV ODER BESCHAFFUNGSPROGRAMM
060306     05  PO-DRAT0408-CTV-MELDUNG                PIC  X(008).
      *
      ***************** ENDE DRAT0408 *********************************
