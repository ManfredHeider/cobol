      *---      *    $Log:   L:/mf/CPY/DRKI1093.CPv  $
      *
      *   Rev 2.4   Dec 14 2004 14:38:08   t13ex58
      *AKI00194: SEB-IMMO Umstellung  Länderkennzeichen
      *
------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1093.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1093.CPv  $      *
      *
      *   Rev 2.3   Dec 14 2004 13:28:26   t13ex58
      *AKI00194: SEB-IMMO Umstellung  Länderkennzeichen
      *
      *   Rev 2.2   03 Apr 2001 15:46:36   T13EX39
      *FKI00469 zus. Land
      *
      *   Rev 2.1   06 Jul 2000 13:27:10   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 1.0   29 Sep 1997 11:54:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 0 9 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER Umsetzung Länder-Kennzeichen ISO-CODE        *
      *    BfG in die Länder-Kennz. der Supporttabelle.                *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM    !ÄNDERUNG                                 !NAME  !VERS.*
      *AUFTRAG  !                                         !      !     *
      *----------------------------------------------------------------*
      *10.05.00 !NEUERSTELLUNG                            !WHO   ! 01  *
      *KI-NEU   !                                         !      !     *
      *         !                                         !      !     *
      * ---------------------------------------------------------------*
      * FKI00469!03.04.01!2.0 ! ZUS. LAND (RU-->SU) AUFGE-!WHO   !     *
      *         !        !    ! NOMMEN                    !      !     *
      *         !        !    !                           !      !     *
      * ---------------------------------------------------------------*
      * AKI00194!28.01.02!3.0 ! ZUS. LAND (TH-->THA)      !WHO   !     *
      *         !        !    !           (KZ-->KZ)       !      !     *
      *         !        !    !           (NA-->NAM)      !      !     *
      *         !        !    !           (ZA-->ZA)       !      !     *
      *         !        !    !           (CN-->PRC)      !      !     *
      *         !        !    !           (AR-->RA)       !      !     *
      *         !        !    !           (NG-->WAN)      !      !     *
      *         !22.04.03!    !           (IL-->IL)       !WMO   !     *
      *         !26.07.04!    !           (HN-->HN)       !WMO   !     *
      *         !03.08.04!    !           (SL-->SL)       !WMO   !     *
      *         !04.11.04!    !           (AE-->UAE)      !WMO   !     *
      *         !        !    !           (AO-->ANG)      !      !     *
      *         !        !    !           (IN-->IND)      !      !     *
      *         !        !    !           (JP-->J)        !      !     *
      *         !        !    !           (TZ-->EAT)      !      !     *
      *         !        !    !                           !      !     *
      ****************************************************************
      * Felder für das Umsetzen des Länderschlüssels
       01  Z-LAND-SCHL           PIC  X(003).
       01  Z-LAND-SCHL-VALUE.

           05 FILLER             PIC  X(006) VALUE 'DE D  '.
           05 FILLER             PIC  X(006) VALUE 'FR F  '.
           05 FILLER             PIC  X(006) VALUE 'NL NL '.
           05 FILLER             PIC  X(006) VALUE 'IT I  '.
           05 FILLER             PIC  X(006) VALUE 'GB GB '.
           05 FILLER             PIC  X(006) VALUE 'IE IRL'.
           05 FILLER             PIC  X(006) VALUE 'DK DK '.
           05 FILLER             PIC  X(006) VALUE 'GR GR '.
           05 FILLER             PIC  X(006) VALUE 'PT P  '.
           05 FILLER             PIC  X(006) VALUE 'ES E  '.
           05 FILLER             PIC  X(006) VALUE 'NO N  '.
           05 FILLER             PIC  X(006) VALUE 'SE S  '.
           05 FILLER             PIC  X(006) VALUE 'FI FIN'.
           05 FILLER             PIC  X(006) VALUE 'CH CH '.
           05 FILLER             PIC  X(006) VALUE 'AT A  '.
           05 FILLER             PIC  X(006) VALUE 'BE B  '.
           05 FILLER             PIC  X(006) VALUE 'LU L  '.

           05 FILLER             PIC  X(006) VALUE 'BR BR '.
           05 FILLER             PIC  X(006) VALUE 'CA CDN'.
           05 FILLER             PIC  X(006) VALUE 'CL RCH'.
           05 FILLER             PIC  X(006) VALUE 'HU H  '.
           05 FILLER             PIC  X(006) VALUE 'IS IS '.
           05 FILLER             PIC  X(006) VALUE 'MX MEX'.
           05 FILLER             PIC  X(006) VALUE 'NZ NZ '.
           05 FILLER             PIC  X(006) VALUE 'US USA'.
           05 FILLER             PIC  X(006) VALUE 'UY ROU'.
           05 FILLER             PIC  X(006) VALUE 'SG SGP'.
           05 FILLER             PIC  X(006) VALUE 'RO RO '.

           05 FILLER             PIC  X(006) VALUE 'CO CO '.
           05 FILLER             PIC  X(006) VALUE 'CZ CZ '.
           05 FILLER             PIC  X(006) VALUE 'DO DOM'.
           05 FILLER             PIC  X(006) VALUE 'CY CY '.
           05 FILLER             PIC  X(006) VALUE 'AU AUS'.
           05 FILLER             PIC  X(006) VALUE 'BG BG '.
           05 FILLER             PIC  X(006) VALUE 'HR HR '.
           05 FILLER             PIC  X(006) VALUE 'MY MAL'.
030401     05 FILLER             PIC  X(006) VALUE 'RU SU '.
280102     05 FILLER             PIC  X(006) VALUE 'TH THA'.
280102     05 FILLER             PIC  X(006) VALUE 'KZ KZ '.
280102     05 FILLER             PIC  X(006) VALUE 'NA NAM'.
280102     05 FILLER             PIC  X(006) VALUE 'ZA ZA '.
280102     05 FILLER             PIC  X(006) VALUE 'CN PRC'.
280102     05 FILLER             PIC  X(006) VALUE 'AR RA '.
280102     05 FILLER             PIC  X(006) VALUE 'NG WAN'.
220403     05 FILLER             PIC  X(006) VALUE 'IL IL '.
260704     05 FILLER             PIC  X(006) VALUE 'HN HN '.
260704     05 FILLER             PIC  X(006) VALUE 'SL WAL'.
041104     05 FILLER             PIC  X(006) VALUE 'AE UAE'.
041104     05 FILLER             PIC  X(006) VALUE 'AO ANG'.
041104     05 FILLER             PIC  X(006) VALUE 'IN IND'.
041104     05 FILLER             PIC  X(006) VALUE 'JP J  '.
041104     05 FILLER             PIC  X(006) VALUE 'TZ EAT'.
       01  Z-LAND-SCHL-ARR REDEFINES Z-LAND-SCHL-VALUE.

           05 Z-LAND-SCHL-OCC                OCCURS 52 .
              10  Z-LAND-SCHL-ISO   PIC  X(003).
              10  Z-LAND-SCHL-KI    PIC  X(003).
       01  Z-LAND-SCHL-MAX          PIC S9(009) VALUE 52 .

      * INDEX FüR DAS UMSETZEN DES LÄNDERSCHLUESSELS
       01  I-LAND-SCHL              PIC  9(004) VALUE 0.


