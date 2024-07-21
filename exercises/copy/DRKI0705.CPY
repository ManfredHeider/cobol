      * P13SP99   Stand: 06/12/98    18:03:28
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0705.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0705.CPv  $
      *
      *   Rev 2.1   Aug 02 1999 14:10:26   T13EX48
      *FKI00094
      *
      *   Rev 2.0   Apr 22 1999 16:51:22   T13EX48
      *AKI0028
      *RDTA
      *
      *   Rev 1.0   06 Oct 1998 15:31:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0705  DATEIAUFBAU RUECKDTA CKV           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    22.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!22.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * Fehler  !01.02.99! 01 ! Vorsatz, Nachsatz korr.   !HSE       *
      * FKI0094 !30.07.99! 02 ! Fehler bei VBNR, da MOVE  !HSE       *
      *         !        !    ! von PIC 9(7) auf 9(6)     !          *
      *         !        !    ! Feld ..GS-CKV umbenannt in!          *
      *         !        !    ! ..DIR-CKV                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      * VORSATZ  : 087  Ab 01.02.1999: 94 HSE                        *
      * HAUPTSATZ: 250                                               *
      * NACHSATZ : 050                                               *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0705-VORSATZ.
           10 DRKI0705-BSA                    PIC X(001).
           10 DRKI0705-BABS                   PIC X(027).
990201*    10 DRKI0705-BERST                  PIC 9(006).
990201     10 DRKI0705-BERST                  PIC 9(008).
990201*    10 DRKI0705-BSTAND                 PIC 9(006).
990201     10 DRKI0705-BSTAND                 PIC 9(008).
           10 DRKI0705-BEMPF                  PIC X(027).
           10 DRKI0705-BINHALT                PIC X(020).
990201     10 DRKI0705-KZ-WAEHR               PIC X(003).
       01  DRKI0705-SATZ.
           10 DRKI0705-BSA-HAUPT              PIC X(001).
           10 DRKI0705-SA-CKV                 PIC X(003).
990730*    10 DRKI0705-GS-CKV                 PIC X(003).
990730     10 DRKI0705-DIR-CKV                PIC X(003).
           10 DRKI0705-RESTA                  PIC X(007) VALUE SPACES.
           10 DRKI0705-VNR-CKV                PIC X(015).
           10 DRKI0705-PARTINF-CKV.
              20 DRKI0705-AIVSNR-CKV          PIC 9(007).
              20 DRKI0705-PRZ-CKV             PIC 9(002).
990730*       20 DRKI0705-PVBNR-CKV           PIC 9(006).
990730        20 DRKI0705-PVBNR-CKV           PIC X(006).
           10 DRKI0705-ERSTELLDT-CKV          pic 9(007) COMP-3.
           10 DRKI0705-RESTB                  PIC X(202) VALUE SPACES.
       01  DRKI0705-NACHSATZ.
           10 DRKI0705-FSA                    PIC X(001).
           10 DRKI0705-FSAETZE                PIC 9(007) COMP-3.
           10 DRKI0705-FSUMME1                PIC 9(017) COMP-3.
           10 DRKI0705-FSUMME2                PIC 9(017) COMP-3.
           10 DRKI0705-FSUMME3                PIC 9(017) COMP-3.
           10 DRKI0705-FSUMME4                PIC 9(017) COMP-3.
           10 DRKI0705-FSUMME5                PIC 9(017) COMP-3.


