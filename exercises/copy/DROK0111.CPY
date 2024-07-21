      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0111.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0111.CPv  $
      *
      *   Rev 2.2   Mar 21 2016 16:39:24   t13ex27
      *FKI01156: Wartungsauftrag Abgleich
      *
      *   Rev 2.1   Mar 22 2010 16:00:12   t13ex27
      *AKI00793: Einarbeitungs-/Einarbeitungszeiten in OKI
      *
      *   Rev 2.0   Mar 22 2010 14:47:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00793!22.03.10!2.1 ! NEU                       !I. Frenzel*
      * FKI01156!17.02.16!2.2 ! DBO-1107 Umbenennung      !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
170216* für OKI_HOST_DML_PACK.SYNCH_VERTR_ROLLE_BEZ_MITVERS
>>    *    05  D-FIRMENKUNDE.
      *        15  FILLER                PIC X(19)
      *                    VALUE '##FIRMENKUNDE'.
      *        15  FILLER                PIC X(01) VALUE SPACE.
      *        15  D-F-VERTRAGID         PIC X(26).
      *        15  FILLER                PIC X(01) VALUE SPACE.
      *        15  D-F-ROLLEN-KZ         PIC X(01).
      *        15  FILLER                PIC X(01) VALUE SPACE.
      *        15  FILLER                PIC X(02) VALUE '##'.

           05  D-VRB-MITVERS-PERSON.
               15  FILLER                PIC X(20)
                           VALUE '##VRB_MITVERS_PERSON'.
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-VMP-VERTRAGID       PIC X(26).
               15  FILLER                PIC X(01) VALUE SPACE.
               15  D-VMP-ROLLEN-KZ       PIC X(01).
    <<         15  FILLER                PIC X(01) VALUE SPACE.
170216         15  FILLER                PIC X(02) VALUE '##'.


