      * P13SP99   Stand: 06/12/98    18:02:56
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0637.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0637.CPv  $
      *
      *   Rev 2.2   Aug 02 1999 14:10:24   T13EX48
      *FKI00094
      *
      *   Rev 2.1   Apr 22 1999 17:02:44   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:24   T13EX48
      * 
      *
      *   Rev 1.0   25 Aug 1998 09:25:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0637  DATEIAUFBAU RUECKDTA CKV           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    22.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!22.09.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * Fehler  !01.02.99! 01 ! Vorsatz, Nachsatz korr.   !          *
      *         !        !    !                           !          *
      * FKI0094 !30.07.99! 01 ! Fehler bei VBNR, da MOVE  !HSE       *
      *         !        !    ! von PIC 9(7) auf 9(6)     !          *
      *         !        !    ! Feld ..GS-CKV umbenannt in!          *
      *         !        !    ! ..DIR-CKV                 !          *
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
       01  DRKI0637-VORSATZ.
           10 DRKI0637-BSA                    PIC X(001).
           10 DRKI0637-BABS                   PIC X(027).
990201*    10 DRKI0637-BERST                  PIC 9(006).
990201     10 DRKI0637-BERST                  PIC 9(008).
990201*    10 DRKI0637-BSTAND                 PIC 9(006).
990201     10 DRKI0637-BSTAND                 PIC 9(008).
           10 DRKI0637-BEMPF                  PIC X(027).
           10 DRKI0637-BINHALT                PIC X(020).
990201     10 DRKI0637-KZ-WAEHR               PIC X(003).
       01  DRKI0637-SATZ.
           10 DRKI0637-BSA-HAUPT              PIC X(001).
           10 DRKI0637-SA-CKV                 PIC X(003).
990730*    10 DRKI0637-GS-CKV                 PIC X(003).
990730     10 DRKI0637-DIR-CKV                PIC X(003).
           10 DRKI0637-RESTA                  PIC X(007) VALUE SPACES.
           10 DRKI0637-VNR-CKV                PIC X(015).
           10 DRKI0637-PARTINF-CKV.
              20 DRKI0637-AIVSNR-CKV          PIC 9(007).
              20 DRKI0637-PRZ-CKV             PIC 9(002).
990730*       20 DRKI0637-PVBNR-CKV           PIC 9(006).
990730        20 DRKI0637-PVBNR-CKV           PIC X(006).
           10 DRKI0637-ERSTELLDT-CKV          pic 9(007) COMP-3.
           10 DRKI0637-RESTB                  PIC X(202) VALUE SPACES.
       01  DRKI0637-NACHSATZ.
           10 DRKI0637-FSA                    PIC X(001).
           10 DRKI0637-FSAETZE                PIC 9(007) COMP-3.
           10 DRKI0637-FSUMME1                PIC 9(017) COMP-3.
           10 DRKI0637-FSUMME2                PIC 9(017) COMP-3.
           10 DRKI0637-FSUMME3                PIC 9(017) COMP-3.
           10 DRKI0637-FSUMME4                PIC 9(017) COMP-3.
           10 DRKI0637-FSUMME5                PIC 9(017) COMP-3.


