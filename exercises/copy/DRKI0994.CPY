      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0994.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0994.CPv  $
      *
      *   Rev 2.2   Oct 26 2009 07:37:06   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *äge
      *
      *   Rev 2.1   Feb 10 2000 12:38:56   T13EX49
      *AKI00070 - Neuerstellung
      *Umsetzen Länderkennzeichen
      *
      *   Rev 2.0   24 Jan 2000 16:36:02   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 9 9 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER Umsetzung Länder-Kennzeichen von             *
      *  BADENIA in die Länder-Kennz. der Supporttabelle.              *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *09.02.00!Neuerstellung (aus DRKI0169)           !P. Pohl  !     *
      *        !                                       !         !     *
121009*12.10.09!Neue Länderkennzeichen                 !WMI      !     *
      *        !                                       !         !     *
      ******************************************************************
      * Felder für das Umsetzen des Länderschlüssels
      * Stelle 1 - 3 -> Länderkennzeichen Badenia
      * Stelle 4 - 6 -> Länderkennzeichen DVAG
       01  Z-LAND-SCHL           PIC  X(003).
       01  Z-LAND-SCHL-VALUE.

           05 FILLER             PIC  X(006) VALUE 'A  A  '.
           05 FILLER             PIC  X(006) VALUE 'AL AL '.
           05 FILLER             PIC  X(006) VALUE 'AUSAUS'.
           05 FILLER             PIC  X(006) VALUE 'B  B  '.
           05 FILLER             PIC  X(006) VALUE 'BD BD '.
           05 FILLER             PIC  X(006) VALUE 'BG BG '.
           05 FILLER             PIC  X(006) VALUE 'BIHBIH'.
           05 FILLER             PIC  X(006) VALUE 'BOLBOL'.
           05 FILLER             PIC  X(006) VALUE 'BR BR '.
           05 FILLER             PIC  X(006) VALUE 'BRUBRU'.
           05 FILLER             PIC  X(006) VALUE 'BURBUR'.
           05 FILLER             PIC  X(006) VALUE 'C  C  '.
           05 FILLER             PIC  X(006) VALUE 'CDNCDN'.
           05 FILLER             PIC  X(006) VALUE 'CH CH '.
           05 FILLER             PIC  X(006) VALUE 'CI CI '.
           05 FILLER             PIC  X(006) VALUE 'CL CL '.
           05 FILLER             PIC  X(006) VALUE 'CO CO '.
           05 FILLER             PIC  X(006) VALUE 'CROHR '.
           05 FILLER             PIC  X(006) VALUE 'CS CZ '.
           05 FILLER             PIC  X(006) VALUE 'CZ CZ '.
           05 FILLER             PIC  X(006) VALUE 'D  D  '.
           05 FILLER             PIC  X(006) VALUE 'DK DK '.
           05 FILLER             PIC  X(006) VALUE 'DUBUAE'.
           05 FILLER             PIC  X(006) VALUE 'DZ DZ '.
           05 FILLER             PIC  X(006) VALUE 'E  E  '.
           05 FILLER             PIC  X(006) VALUE 'EAKEAK'.
           05 FILLER             PIC  X(006) VALUE 'EATEAT'.
           05 FILLER             PIC  X(006) VALUE 'EC EC '.
           05 FILLER             PIC  X(006) VALUE 'ET ET '.
           05 FILLER             PIC  X(006) VALUE 'F  F  '.
121009     05 FILLER             PIC  X(006) VALUE 'FINFIN'.
           05 FILLER             PIC  X(006) VALUE 'FJIFJI'.
           05 FILLER             PIC  X(006) VALUE 'GB GB '.
           05 FILLER             PIC  X(006) VALUE 'GCAGCA'.
           05 FILLER             PIC  X(006) VALUE 'GH GH '.
           05 FILLER             PIC  X(006) VALUE 'GR GR '.
           05 FILLER             PIC  X(006) VALUE 'GUSRUS'.
           05 FILLER             PIC  X(006) VALUE 'H  H  '.
           05 FILLER             PIC  X(006) VALUE 'HK HK '.
121009     05 FILLER             PIC  X(006) VALUE 'HR HR '.
           05 FILLER             PIC  X(006) VALUE 'I  I  '.
           05 FILLER             PIC  X(006) VALUE 'IL IL '.
           05 FILLER             PIC  X(006) VALUE 'INDIND'.
           05 FILLER             PIC  X(006) VALUE 'IR IR '.
           05 FILLER             PIC  X(006) VALUE 'IRLIRL'.
           05 FILLER             PIC  X(006) VALUE 'IRQIRQ'.
           05 FILLER             PIC  X(006) VALUE 'IS IS '.
           05 FILLER             PIC  X(006) VALUE 'J  J  '.
           05 FILLER             PIC  X(006) VALUE 'JORJOR'.
121009     05 FILLER             PIC  X(006) VALUE 'KSAKSA'.
           05 FILLER             PIC  X(006) VALUE 'KWTKWT'.
           05 FILLER             PIC  X(006) VALUE 'L  L  '.
           05 FILLER             PIC  X(006) VALUE 'LARLAR'.
121009     05 FILLER             PIC  X(006) VALUE 'LT LT '.
           05 FILLER             PIC  X(006) VALUE 'M  M  '.
           05 FILLER             PIC  X(006) VALUE 'MA MA '.
           05 FILLER             PIC  X(006) VALUE 'MALMAL'.
           05 FILLER             PIC  X(006) VALUE 'MC MC '.
           05 FILLER             PIC  X(006) VALUE 'MD MD '.
           05 FILLER             PIC  X(006) VALUE 'MEXMEX'.
           05 FILLER             PIC  X(006) VALUE 'MON?  '.
           05 FILLER             PIC  X(006) VALUE 'MZDMK '.
           05 FILLER             PIC  X(006) VALUE 'N  N  '.
           05 FILLER             PIC  X(006) VALUE 'NA NA '.
           05 FILLER             PIC  X(006) VALUE 'NICNIC'.
           05 FILLER             PIC  X(006) VALUE 'NL NL '.
           05 FILLER             PIC  X(006) VALUE 'NZ NZ '.
           05 FILLER             PIC  X(006) VALUE 'P  P  '.
           05 FILLER             PIC  X(006) VALUE 'PAKPK '.
           05 FILLER             PIC  X(006) VALUE 'PL PL '.
121009     05 FILLER             PIC  X(006) VALUE 'PRCPRC'.
           05 FILLER             PIC  X(006) VALUE 'PY PY '.
           05 FILLER             PIC  X(006) VALUE 'R  RO '.
           05 FILLER             PIC  X(006) VALUE 'RA RA '.
           05 FILLER             PIC  X(006) VALUE 'RC RC '.
           05 FILLER             PIC  X(006) VALUE 'RCHRCH'.
           05 FILLER             PIC  X(006) VALUE 'RH RH '.
           05 FILLER             PIC  X(006) VALUE 'RI RI '.
121009     05 FILLER             PIC  X(006) VALUE 'RO RO '.
           05 FILLER             PIC  X(006) VALUE 'ROKROK'.
           05 FILLER             PIC  X(006) VALUE 'ROUROU'.
           05 FILLER             PIC  X(006) VALUE 'RP RP '.
121009     05 FILLER             PIC  X(006) VALUE 'RUSRUS'.
           05 FILLER             PIC  X(006) VALUE 'S  S  '.
121009     05 FILLER             PIC  X(006) VALUE 'SCGSCG'.
           05 FILLER             PIC  X(006) VALUE 'SD SD '.
           05 FILLER             PIC  X(006) VALUE 'SF FIN'.
           05 FILLER             PIC  X(006) VALUE 'SGPSGP'.
           05 FILLER             PIC  X(006) VALUE 'SLOSLO'.
           05 FILLER             PIC  X(006) VALUE 'SON?  '.
           05 FILLER             PIC  X(006) VALUE 'SRBYU '.
           05 FILLER             PIC  X(006) VALUE 'SU SU '.
           05 FILLER             PIC  X(006) VALUE 'SYRSYR'.
           05 FILLER             PIC  X(006) VALUE 'T  THA'.
121009     05 FILLER             PIC  X(006) VALUE 'THATHA'.
           05 FILLER             PIC  X(006) VALUE 'TN TN '.
           05 FILLER             PIC  X(006) VALUE 'TR TR '.
121009     05 FILLER             PIC  X(006) VALUE 'UAEUAE'.
           05 FILLER             PIC  X(006) VALUE 'USAUSA'.
           05 FILLER             PIC  X(006) VALUE 'VAEUAE'.
           05 FILLER             PIC  X(006) VALUE 'VENYV '.
           05 FILLER             PIC  X(006) VALUE 'VN VN '.
           05 FILLER             PIC  X(006) VALUE 'WANWAN'.
121009     05 FILLER             PIC  X(006) VALUE 'WL WL '.
           05 FILLER             PIC  X(006) VALUE 'Z  Z  '.
           05 FILLER             PIC  X(006) VALUE 'ZA ZA '.

       01  Z-LAND-SCHL-ARR REDEFINES Z-LAND-SCHL-VALUE.

121009*    05 Z-LAND-SCHL-OCC                OCCURS 95.
121009     05 Z-LAND-SCHL-OCC                OCCURS 106.
              10  Z-LAND-SCHL-BADENIA  PIC  X(003).
              10  Z-LAND-SCHL-KI       PIC  X(003).
121009*01  Z-LAND-SCHL-MAX             PIC S9(009) VALUE 95.
121009 01  Z-LAND-SCHL-MAX             PIC S9(009) VALUE 106.

      * INDEX FüR DAS UMSETZEN DES LÄNDERSCHLUESSELS
       01  I-LAND-SCHL                 PIC  9(004) VALUE 0.


