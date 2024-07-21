      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0091.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0091.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:51:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0091, SCHNITTSTELLE UPAT0010             *
      *                 ERMITTLUNG QUARTAL UND MONATE DES QUARTALS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: SCHÖLLHAMMER                                    *
      *    DATUM:    14.02.1995                                      *
      ****************************************************************
      *    VERSION   1    10.03.95 09:58:50
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0183!14.02.95!    ! NEU                       ! SCHÖLLH. *
      *         !10.03.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *
      *   PI-UPAT0010
      *
      *     FKT-AKT-QUARTAL      DATEN ZUM AKTUELLEN QUARTAL MIT
      *                             BEZUGSDATUM = TAGESDATUM
      *     FKT-VOR-QUARTAL      DATEN ZUM VERGANGENEN QUARTAL MIT
      *                             BEZUGSDATUM = TAGESDATUM
      *                          !! ACHTUNG: AB DEM 26. DES LETZTEN
      *                             QUARTALSMONATS GILT EIN QUARTAL
      *                             ALS VERGANGEN (Z.B. IST AM 26.3.95
      *                             DAS QUARTAL 1/95 DAS VOR-QUARTAL)
      *     FKT-QUARTAL-ZU-DAT   DATEN ZUM DEM QUARTAL IN DEM DAS
      *                             ÜBERGEBENE ZU-DATUM LIEGT
      *
      *     ZU-DATUM-ISO         NUR BEI FKT-QUARTAL-ZU-DAT ZU FÜLLEN
      *
      *   PO-UPAT0010
      *
      *     RC-OK                   NORMALE VERARBEITUNG
      *     RC-VOR-QUART-VORZEITIG  VERARBEITUNG OK, ABER VORQUARTAL
      *                                VORZEITIG BEENDET
      *                                (NUR BEI FKT-VOR-QUARTAL MÖGLICH)
      *
      *     QUARTAL-Q          QUARTAL ALS ZIFFER (1/2/3/4)
      *     QUARTAL-JAHR       JAHR DES QUARTALS: VIERSTELLIG
      *     MONAT-1-ISO        LETZTER (!!!) TAG DES 1. QUARTALSMONATS
      *     MONAT-1-TEXT-GR    NAME DES 1. MONATS IN GROSSSCHREIBUNG
      *     MONAT-1-TEXT-GKL   NAME DES 1. MONATS IN GROSS/KLEINSCHREIB.
      *

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPAT0010.
           05  PI-UPAT0010-FKT                PIC  X(02).
               88  PI-AT10-FKT-AKT-QUARTAL               VALUE 'AQ'.
               88  PI-AT10-FKT-VOR-QUARTAL               VALUE 'VQ'.
               88  PI-AT10-FKT-QUARTAL-ZU-DAT            VALUE 'QD'.

           05  PI-UPAT0010-ZU-DATUM-ISO       PIC  X(10).
           05  FILLER                         PIC  X(20).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPAT0010.
           05  PO-UPAT0010-RC                 PIC  X(03).
               88  PO-AT10-RC-OK                         VALUE 'OK '.
               88  PO-AT10-RC-VOR-QUART-VORZEITIG        VALUE 'VQV'.

           05  PO-UPAT0010-QUARTAL.
               10 PO-UPAT0010-QUARTAL-Q       PIC  9(01).
               10 PO-UPAT0010-QUARTAL-JAHR    PIC  9(04).
           05  PO-UPAT0010-MONAT-1-ISO        PIC  X(10).
           05  PO-UPAT0010-MONAT-2-ISO        PIC  X(10).
           05  PO-UPAT0010-MONAT-3-ISO        PIC  X(10).
           05  PO-UPAT0010-MONAT-1-TEXT-GR    PIC  X(09).
           05  PO-UPAT0010-MONAT-2-TEXT-GR    PIC  X(09).
           05  PO-UPAT0010-MONAT-3-TEXT-GR    PIC  X(09).
           05  PO-UPAT0010-MONAT-1-TEXT-GKL   PIC  X(09).
           05  PO-UPAT0010-MONAT-2-TEXT-GKL   PIC  X(09).
           05  PO-UPAT0010-MONAT-3-TEXT-GKL   PIC  X(09).

           05  FILLER                         PIC  X(36).

      ******** ENDE COPY-MEMBER ************************************

