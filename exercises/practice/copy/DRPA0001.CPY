      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:44:28   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00012000
      *    COPY-MEMBER: DRPA0001                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE UPPA0001 LESE-OZP STEUERUNG_DTA              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       * 00060000
      *    DATUM:    16.01.1996                                       * 00070000
      ****************************************************************  00091000
      *    VERSION
      ****************************************************************  00091200
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !        !    ! NEU                       ! MUELLER  *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *   - ERLÄUTERUNG DER FUNKTIONEN
      *   - WELCHE EINGABEN (PI) ERFORDERLICH (ABH. VON FKT)
      *   - WELCHE AUSGABEN (PO) WERDEN GELIEFERT
      *        (ABH. VON FKT UND RC)
      *   - EVTL. ERLÄUTERUNG ANDERER PI- ODER PO-FELDER
      *
      ****************************************************************  00099600

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPPA0001.
           05  PI-UPPA0001-FKT                PIC  X(02).
               88  PI-PA01-FKT-1                         VALUE '01'.
               88  PI-PA01-FKT-2                         VALUE '02'.
               88  PI-PA01-FKT-3                         VALUE '03'.
           05 PI-UPPA0001-P-ID-INTERN-PG      PIC  9(04).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPPA0001.
           05  PO-UPPA0001-RC                 PIC  X(03).
               88  PO-PA01-RC-OK                         VALUE 'OK '.
               88  PO-PA01-RC-NOT-FOUND                  VALUE 'NFD'.
               88  PO-PA01-RC-NOK                        VALUE 'NOK'.
           05 PO-UPPA0001-P-ID-INTERN-PG      PIC  9(04).
           05 PO-UPPA0001-FREIGABE            PIC  X(01).
           05 PO-UPPA0001-ANZAHL-FEHLER       PIC S9(07) PACKED-DECIMAL.
           05 PO-UPPA0001-PROZ-SATZ-FEHLER    PIC  9(03).
           05 PO-UPPA0001-AENDERUNGSDATEN.
              10 PO-UPPA0001-AEND-ZUORDNUNG   PIC  X(01).
              10 PO-UPPA0001-AEND-SYSZEIT     PIC  X(26).
              10 PO-UPPA0001-AEND-KENNUNG     PIC  X(08).
           05 PO-UPPA0001-AEND-SYSZEIT-DV     PIC  X(20).
      *
      ******** ENDE COPY-MEMBER (DRPA0001) *******************

