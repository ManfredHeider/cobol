      * T13EX33   Stand: 21/09/98    15:33:49
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0352.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0352.CPv  $
      *
      *   Rev 2.4   Sep 07 2016 13:18:00   t13ex48
      *AKI01462: Korrespondenz-Makler AML: Abgang aufgrund von Makl
      *erbetreuung: COPYs
      *
      *   Rev 2.3   Jul 02 2015 16:54:44   t13ex27
      *AKI01373: Einmalzahlung für DVAG Wien-Programme
      *
      *   Rev 2.2   Sep 02 1999 09:24:24   T13AE38
      *AKI00044
      *
      *   Rev 2.1   Mar 26 1999 13:53:08   T13EX33
      *AKI00001
      *
      *   Rev 2.0   Mar 23 1999 15:52:32   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:01:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 5 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *                                                                *
      *    SATZLÄNGE   81                                              *
010916*    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.07.98!NEUERSTELLUNG                          !HUEBNER  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *31.08.99!ÄNDERUNG                               !S.JOST   ! 02  *
      *AKI00044!EURO UMSTELLUNG  WÄHRUNGSKENNZEICHEN   !         !     *
      *        !PIC X(001) EINGEFÜGT                   !         !     *
      *        !SOWIE WAEHRUNGSÄNDERUNG-KZ             !         !     *
      *        !                                       !         !     *
      *--------!--------!----!------------------------------!----------*
010916*AKI01462!01.09.16! 03 ! Korrespondnz-Makler-KZ       ! HSE      *
      *        !        !    ! aufgenommen. Neuer Vertrags- !          *
      *        !        !    ! zustand bei DELETE.          !          *
      *        !        !    ! FILLER eingefuegt            !          *
      *--------!--------!----!------------------------------!----------*
      ******************************************************************
 
           02  'XXX'-VERTRAG.
           05  'XXX'-GES                    PIC  9(02).
           05  'XXX'-VNR                    PIC  9(15).
      *
           05  'XXX'-VBNR                   PIC  X(07).
           05  'XXX'-AEND-VBNR-KZ           PIC  X(01).
      *        J WENN VBNR GEAENDERT
 
           05  'XXX'-ZAHLART                PIC S9(02).
           05  'XXX'-AEND-ZAHLART-KZ        PIC  X(01).
      *        J WENN ZAHLWEISE GEAENDERT
 
           05  'XXX'-ZAHLWEISE              PIC S9(02).
           05  'XXX'-AEND-ZAHLWEISE-KZ      PIC  X(01).
      *        J WENN ZAHLWEISE GEAENDERT
 
           05  'XXX'-VERTRAGSZUSTAND        PIC S9(02).
           05  'XXX'-AEND-VTGZUSTAND-KZ     PIC X(01).
      *        J WENN VERTRAGSZUSTAND GEAENDERT
 
           05  'XXX'-VERTRAGSZUSTAND-DATUM  PIC  X(10).
           05  'XXX'-AEND-VTGZUSTDAT-KZ     PIC X(01).
      *        J WENN VERTRAGSZUSTAND DATUM GEAENDERT
 
           05  'XXX'-VAEND-DAT              PIC  X(10).
           05  'XXX'-AEND-VAEND-DAT-KZ      PIC  X(01).
      *        J WENN VAEND-DAT GEAENDERT
 
           05  'XXX'-VERTRAGSBEGINN         PIC  X(10).
           05  'XXX'-AEND-VERTRAGSBEGINN-KZ PIC  X(01).
      *        J WENN VERTRAGSBEGINN GEAENDERT
 
           05  'XXX'-VERTRAGSABLAUF         PIC  X(10).
           05  'XXX'-AEND-VERTRAGSABLAUF-KZ PIC  X(01).
      *        J WENN VERTRAGSABLAUF GEAENDERT
 
310899     05  'XXX'-WAEHRUNGKZ             PIC  X(01).
310899     05  'XXX'-AEND-WAEHRUNGKZ        PIC  X(01).
310899*        J WENN WAEHRUNGSÄNDERUNG
 
010916     05  'XXX'-KORR-MAKLERKZ          PIC  X(01).
010916     05  'XXX'-AEND-KORR-MAKLERKZ     PIC  X(01).
010916*        J WENN Aenderung des Makler-KZ
 
           05  'XXX'-AKTUAL-KZ              PIC  X(01).
 
010916     05  FILLER                       PIC  X(17).

