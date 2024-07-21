      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0050.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0050.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Mar 20 1997 16:59:46   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0050       DIALOG-RAHMENSTEUERUNG         *
      *     DAS COPY ENTHäLT 2 DUMMY-SECTIONS, ZUM USER-DATEN         *
      *     SICHERN/ENTSICHERN, DIE VOM DIALOG-RAHMEN STANDARD-       *
      *     MäßIG AUFGERUFEN WERDEN.                                  *
      *     WERDEN DIE SECTIONS IM DIALOG BENöTIGT, MUß DIESES        *
      *     COPY ENTFERNT WERDEN, DA DIE SECTIONS SONST DOPPELT SIND. *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R. AUERBACH                                      *
      *    DATUM:    21.01.1997                                       *
      *****************************************************************
      *    VERSION   1    21.01.97 16:37:15
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!21.01.97!    ! NEU                       ! AUERBACH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *------------------------------------------------------------*
       DS-USERDATEN-SICHERN SECTION.
       DS-ANF.
      *------------------------------------------------------------*
      * INPUT :                                                    *
      * OUTPUT:                                                    *
      * VERARBEITUNG:                                              *
      *   - DUMMY-SECTION                                          *
      *------------------------------------------------------------*
      *
           CONTINUE
           .
       DS-EXIT.
           EXIT.

      *------------------------------------------------------------*
       DE-USERDATEN-ENTSICHERN SECTION.
       DE-ANF.
      *------------------------------------------------------------*
      * INPUT :                                                    *
      * OUTPUT:                                                    *
      * VERARBEITUNG:                                              *
      *   - DUMMY-SECTION                                          *
      *------------------------------------------------------------*
      *
           CONTINUE
           .
       DE-EXIT.
           EXIT.

      *
      ******* ENDE COPY-MEMBER DPAT0050 (DIALOG-RAHMENSTEUERUNG) ******

