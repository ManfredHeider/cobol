      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0132.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0132.CPv  $
      *
      *   Rev 2.1   Nov 22 2012 14:59:02   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.0   Sep 24 2012 11:51:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOK00051!24.09.12!2.1 ! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  D-DELETE-VERTRAG.
               15  FILLER                PIC X(26)
                           VALUE '##BEFOR_COMPLETE_VTRLOESCH'.
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-BCV-VERTRAGID       PIC X(26).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  FILLER                PIC X(02) VALUE '##'.
      *
      ********** ENDE COPY-MEMBER **************************************
