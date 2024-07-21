      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0144.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0144.CPv  $
      *
      *   Rev 2.1   Sep 08 2004 08:43:40   t13ae34
      *AAN00506: UU52 - Hilfefenster VSNR
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Jul 1997 09:48:08   T13AE34
      *AFAN0146 - Driver-Anpassung Alt-Neudialoge
      *
      *   Rev 1.0   Feb 19 1997 12:21:56   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0144                                      *
      *    --------------------------------------------------------   *
      *    REDEFINES FüR TCTUA-AREA/TS-FILLER (-> COPY UU910S.CPY)    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    22.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFAN0146!22.07.97!    ! NEU                       ! HEIDER   *
      * AAN00506!27.08.04!2.1 ! HILFEBILD UU52            ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DAS REDEFINES VON UU910S/TS-FILLER
      *    FÜR DIE KOMMUNIKATION ALT-NEUDIALOGE
      *
      ****************************************************************
      *
           02  DRAT0144-FILLER REDEFINES TS-FILLER.
      *        DRIVER-OP-CODE
              03  DRAT0144-CADR-E-OP               PIC X(003).
      *        RüCKKEHR-TRANSAKTION
              03  DRAT0144-CARD-DIALOG-SCHRITT     PIC X(004).
      *        KOMMUNIKATIONSBEREICH
              03  DRAT0144-CADD-KOMM-VAR           PIC X(128).
      *        REST
270804*       03  DRAT0144-REST                    PIC X(029).
270804        03  DRAT0144-REST                    PIC X(027).
270804        03  DRAT0144-CARD-BILDNR             PIC 9(002).
      *
