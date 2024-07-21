      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Nov 15 1995 16:35:48   T13EX11
      *AFVB0226 Schnittstelle nachdokumentiert
      *
      *   Rev 1.0   Nov 04 1995 09:04:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0007                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0013 MA-INFO-STAMM-DATEN                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ==> DRVB0001 BEACHTEN BEI ÄNDERUNGEN!!                     * 00051000
      *    --------------------------------------------------------   * 00052000
      *    ERSTELLT: GIESE                                            * 00052100
      *    DATUM:    23.04.1993                                       * 00052200
      ***************************************************************** 00052400
      *    VERSION   02   16.09.93                                   *  00052500
      ***************************************************************** 00052600
      * AENDERUNGEN:                                                 *  00052800
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00052900
      * -------------------------------------------------------------*  00053000
      *         !23.04.93! 01 ! NEU                       ! GIESE    *  00053100
      *         !16.09.93! 02 ! PLZ-ANPASSUNG             ! GIESE    *  00053200
      * AFVB0226!01.08.95!    ! SCHNITTST NACHDOKUMENTIERT! SCHÖLLH. *  00053300
      *         !07.11.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *  00053403
      *         !        !    !                           !          *  00053500
      *         !        !    !                           !          *  00053600
      *         !        !    !                           !          *  00053700
      *         !        !    !                           !          *  00053800
      *         !        !    !                           !          *  00053900
      *                                                              *  00054000
      * ENDE-LOGBUCH                                                 *  00054100
      ****************************************************************  00054200
      *                                                                 00054300
      * ACHTUNG !!!                                                     00054402
      *    UPRO Z. ZT. (STAND 1.8.1995) NICHT FÜR ST-TYP 'ED'           00054502
      *    VERWENDBAR, DA ES ABBRECHEN WÜRDE                            00054602
      *                                                                 00054902
      * EINGABE:                                                        00055001
      *    PI-UPVB0013-ST-NR         MUSS, (SIEHE ST-TYP !)             00055102
      *    PI-UPVB0013-ST-PRZ        KANN, (NICHT AUSGEWERTET)          00055202
      *    PI-UPVB0013-ST-TYP        MUSS, GÜLTIG: MA, ED, RFT, KL      00055302
      *    PI-UPVB0013-CICS-DAT-ISO  MUSS, FÜR ERMITTLUNG 'TÄTIG'       00055402
      *                                                                 00055500
      * AUSGABE:                                                        00055601
      *    PO-UPVB0013-MA-TYP-BEZ    KNR-BEZ-KURZ FÜR MA-TYP BZW ST-TYP 00055701
      *    PO-UPVB0013-MA-NAME       AKT. (!) NAME AUS MAN              00055901
      *    PO-UPVB0013-MA-VORNAME    AKT. (!) VORNAME AUS MAN           00056101
      *    PO-UPVB0013-MAAN-PLZ      AKT. (!) PLZ AUS PRIVATANSCHRIFT   00056201
      *    PO-UPVB0013-MAAN-ORT      DTO.                               00056301
      *    PO-UPVB0013-MAAN-STRASSE  DTO.                               00056401
      *    PO-UPVB0013-DAT-BIS-ISO   BIS-DAT DER LETZTE TÄTIGKEIT ZUM   00056501
      *                                 ÜBERGEBENEN CICS-DAT-ISO        00056701
      *                                 (FALLS VORH.)                   00056801
      *    PO-UPVB0013-MA-TAETIG     KENNZEICHEN: J = TÄTIG, N = UNTÄTIG00057001
      *                                 ZUM ÜBERGEBENEN CICS-DAT-ISO    00057101
      *                                                                 00057200
      * ABHÄNGIGKEIT DER AUSGABEN VOM ST-TYP:                           00057301
      *                                    ! MA  ! RFT ! KL  ! ED  !    00057501
      *    --------------------------------+-----+-----+-----+-----+    00057601
      *    PO-UPVB0013-MA-TYP-BEZ          !     !     !     !     !    00057701
      *       - KNR-BEZ-KURZ FÜR MA-TYP    !  X  !     !     !     !    00057801
      *       - KNR-BEZ-KURZ FÜR ST-TYP    !     !  X  !  X  !  X  !    00057901
      *    PO-UPVB0013-MA-NAME             !  X  !  X  !  X  !   ? !    00058001
      *    PO-UPVB0013-MA-VORNAME          !  X  !  X  !  X  !   ? !    00058201
      *    PO-UPVB0013-MAAN-PLZ            !  X  !  X  !  X  !   ? !    00058301
      *    PO-UPVB0013-MAAN-ORT            !  X  !  X  !  X  !   ? !    00058401
      *    PO-UPVB0013-MAAN-STRASSE        !  X  !  X  !  X  !   ? !    00058501
      *    PO-UPVB0013-DAT-BIS-ISO         !  X  !  X  !  X  !  X  !    00058601
      *    PO-UPVB0013-MA-TAETIG           !  X? !  X? !  X? !  X? !    00058701
      *                                                                 00058901
      *    ANMERKUNGEN:                                                 00059001
      *      '?'  UPVB0013 WÜRDE BEI ST-TYP ED ABSTÜRZEN                00059101
      *      'X?' UPVB0013 LIEFERT KEIN ZUVERLÄSSIGES KZ ZURÜCK         00059201
      *                    (WG. FEHLER IN UPVB0018)                     00059301
      *                                                                 00059401
       01  PI-UPVB0013.                                                 00059500
           05 PI-UPVB0013-ST-NR          PIC S9(09) COMP.               00059600
           05 PI-UPVB0013-ST-PRZ         PIC  X(01).                    00059700
           05 PI-UPVB0013-ST-TYP         PIC S9(04) COMP.               00059800
           05 PI-UPVB0013-CICS-DAT-ISO   PIC  X(10).                    00059900
      *                                                                 00060000
       01  PO-UPVB0013.                                                 00060100
           05 PO-UPVB0013-MA-TYP-BEZ     PIC X(02).                     00060200
           05 PO-UPVB0013-MA-NAME        PIC X(40).                     00060300
           05 PO-UPVB0013-MA-VORNAME     PIC X(40).                     00061000
           05 PO-UPVB0013-MAAN-PLZ       PIC X(05).                     00070000
           05 PO-UPVB0013-MAAN-ORT       PIC X(40).                     00080000
           05 PO-UPVB0013-MAAN-STRASSE   PIC X(40).                     00090000
           05 PO-UPVB0013-DAT-BIS-ISO    PIC X(10).                     00100000
           05 PO-UPVB0013-MA-TAETIG      PIC X(01).                     00110000
      *                                                                 00120000
      ******** ENDE COPY-MEMBER (DRVB0007) *******************          00130000
