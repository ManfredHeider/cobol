      * T13AE38   Stand: 30/08/98    13:45:12
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0339.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0339.CPv  $
      *
      *   Rev 2.3   Sep 07 2016 13:17:54   t13ex48
      *AKI01462: Korrespondenz-Makler AML: Abgang aufgrund von Makl
      *erbetreuung: COPYs
      *
      *   Rev 2.2   Sep 02 1999 09:44:20   T13AE38
      *
      *
      *   Rev 2.1   Mar 25 1999 09:19:54   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:00:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *        - Ausgabe Allg. Vertragsdaten (alle Gesellschaften) -   *
      *                                                                *
      *    SATZLÄNGE   054                                             *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.07.98!NEUERSTELLUNG                          !S.JOST   ! 01  *
      *AKI00001!                                       !         !     *
      *30.08.99!ÄNDERUNG                               !S.JOST   ! 02  *
      *AKI00044!EURO UMSTELLUNG  WÄHRUNGSKENNZEICHEN   !         !     *
      *        !PIC X(003) EINGEFÜGT                   !         !     *
      *--------!--------!----!------------------------------!----------*
010916*AKI01462!01.09.16! 03 ! Korrespondnz-Makler-KZ       ! HSE      *
      *        !        !    ! aufgenommen. Neuer Vertrags- !          *
      *        !        !    ! zustand bei DELETE.          !          *
      *--------!--------!----!------------------------------!----------*
      ******************************************************************
      *
       01  AML-VERTRAG-SATZ.
           05  V-KEY.
               10  V-GES                     PIC 9(02).
               10  V-VNR                     PIC 9(15).
      *
           05  V-VERTRAGSDATEN.
               10  V-VBNR                    PIC X(07).
               10  V-AEND-VBNR-KZ            PIC X(01).
      *
               10  V-ZAHLART                 PIC 9(02).
               10  V-AEND-ZAHLART-KZ         PIC X(01).
               10  V-ZAHLWEISE               PIC 9(02).
               10  V-AEND-ZAHLWEISE-KZ       PIC X(01).
               10  V-VAEND-DAT               PIC X(08).
300899         10  V-WAEHRUNG-KZ             PIC X(03).
300899         10  V-AEND-WAEHRUNG-KZ        PIC X(01).
010916         10  V-KORR-MAKLER-KZ          PIC X(01).
010916         10  V-AEND-KORR-MAKLER-KZ     PIC X(01).
      *
           05  V-AKTUAL-KZ                   PIC  X(01).
010916*    05  V-RESERVE                     PIC  X(10).
010916     05  V-RESERVE                     PIC  X(08).

