      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0080.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0080.CPv  $
      *
      *   Rev 2.1   Jun 01 1999 11:18:52   T13AE30
      *AVB438 - Änderung Schnittstelle F&A
      *
      *   Rev 2.0   Feb 27 1999 11:57:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:45:58   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Apr 03 1997 16:22:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KONSTANTEN FUER ALTE STUFEN (DRAT0080)        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    20.02.1997                                       *
      *****************************************************************
      *    VERSION   01   20.04.94                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * daisy   !20.02.97!    ! neu                       ! giese    *
      * AVB00438!02.02.99!    ! Anpassung wg. zweideutig- ! WILLIG   *
      *         !        !    ! keit der Namen            !          *  
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
      *   AUSPRÄGUNGEN STUFE  (==> UU915S)
      *
       01  K-STF-CPD                PIC S9(04) COMP VALUE 999.
       01  K-STF-RFA                PIC S9(04) COMP VALUE 890.
       01  K-STF-RFR                PIC S9(04) COMP VALUE 889.
       01  K-STF-RFT                PIC S9(04) COMP VALUE 888.
       01  K-STF-KL                 PIC S9(04) COMP VALUE 887.
       01  K-STF-ED                 PIC S9(04) COMP VALUE 768.
      *
      *
      ******************************************************************
      *   ERWEITERUNG DER NAMEN IN K-ALT-..., DA BEI GLEICHZEITIGER
      *   NUTZUNG VON NEUEN UND ALTEN STUFEN-KONSTANTEN KEINE OFFEN-
      *   SICHTLICHE EINDEUTIGKEIT BESTEHT.
      *
      *   BITTE ZUKÜNFTIG DIE NEUEN, ERWEITERTEN KONSTANTEN VERWENDEN!!!
      *
      ******************************************************************
      *
       01  K-ALT-STF-VM09               PIC S9(04) COMP VALUE 9.
       01  K-ALT-STF-VBA1               PIC S9(04) COMP VALUE 16.
       01  K-ALT-STF-VBA2               PIC S9(04) COMP VALUE 18.
       01  K-ALT-STF-VBA3               PIC S9(04) COMP VALUE 19.
       01  K-ALT-STF-ED                 PIC S9(04) COMP VALUE 768.
       01  K-ALT-STF-KL                 PIC S9(04) COMP VALUE 887.
       01  K-ALT-STF-RFT                PIC S9(04) COMP VALUE 888.
       01  K-ALT-STF-RFR                PIC S9(04) COMP VALUE 889.
       01  K-ALT-STF-RFA                PIC S9(04) COMP VALUE 890.
       01  K-ALT-STF-CPD                PIC S9(04) COMP VALUE 999.


      ******** ENDE COPY-MEMBER KONSTANTEN ALTE STUFEN (DRAT0080)  ***
