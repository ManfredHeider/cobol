      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:09:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00010000
      *    COPY-MEMBER: DRVB0032                                     *  00020000
      *    --------------------------------------------------------  *  00030000
      *    SCHNITTSTELLE UPVB0032 PRÜFEN DATENTYP 'PROZENTSATZ'      *  00040000
      *    --------------------------------------------------------  *  00050000
      *    ERSTELLT: SCHMIDT                                         *  00060000
      *    DATUM:    24.04.1993                                      *  00070000
      ****************************************************************  00080000
      *    VERSION   02   15.10.93                                   *  00090001
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      *         !24.04.93! 01 ! NEU                       ! SCHMIDT  *  00140000
      *         !13.10.93!    ! AUSBAU UPVB0032 ZUM ALLGE-! SCHÖLLH. *  00150000
      *         !        !    !  MEINEN PRÜF-PGM FÜR DEN  !          *  00160000
      *         !        !    !  DATENTYP 'PROZENTSATZ'   !          *  00170000
      *         !        !    !  (ALLES NEU; NICHT MARK.) !          *  00180000
      *         !15.10.93! 02 ! PROD.ÜBERGABE             ! HECKER   *  00181001
      *         !        !    !                           !          *  00190000
      *         !        !    !                           !          *  00200000
      *         !        !    !                           !          *  00210000
      *                                                              *  00220000
      * ENDE-LOGBUCH                                                 *  00230000
      ****************************************************************  00240000
      *                                                                 00250000
      *    FUNKTION DES UPVB0032:                                       00260000
      *      - PRÜFEN PROZENTSATZ AUF FORMALE KORREKTHEIT               00270000
      *        (NUMERISCH; NICHT ZU VIELE VORKOMMA-/NACHKOMMASTELLEN)   00280000
      *      - KONVERTIEREN DER EINGABE IN                              00290000
      *          - NUMERISCHE FORM                                      00300000
      *          - DRUCKAUFBEREITETE FORM                               00310000
      *      - FALLS NICHT UPVB0032-FKT-NUR-FORMAL:                     00320000
      *          - FACHLICHE PRÜFUNG ABH. VON FKT UND VARIANTE          00330000
      *                                                                 00340000
      *    EINGABE:                                                     00350000
      *      IMMER:                                                     00360000
      *        - PI-UPVB0032-FKT                                        00370000
      *        - PI-UPVB0032-PROZENTSATZ-X                              00380000
      *      FALLS FKT UNGLEICH 'NUR-FORMAL' IST:                       00390000
      *        - PI-UPVB0032-VARIANTE                                   00400000
      *                                                                 00410000
      *    AUSGABE:                                                     00420000
      *      IMMER:                                                     00430000
      *        - PO-UPVB0032-RC                                         00440000
      *      FALLS PROZENTSATZ FORMAL KORREKT:                          00450000
      *        - PO-UPVB0032-PROZENTSATZ-NUM                            00460000
      *        - PO-UPVB0032-PROZENTSATZ-DR                             00470000
      *                                                                 00480000
      *                                                                 00490000
       01  PI-UPVB0032.                                                 00500000
           05  PI-UPVB0032-FKT              PIC  9(002).                00510000
               88  UPVB0032-FKT-NUR-FORMAL              VALUE 1.        00520000
               88  UPVB0032-FKT-RCKSTZ                  VALUE 2.        00530000
               88  UPVB0032-FKT-PROVSTZ                 VALUE 3.        00540000
           05  PI-UPVB0032-VARIANTE         PIC  9(002).                00550000
               88  UPVB0032-VARIANTE-1                  VALUE 1.        00560000
                                                                        00570000
           05  PI-UPVB0032-PROZENTSATZ-X    PIC  X(006).                00580000
      *                                                                 00590000
       01  PO-UPVB0032.                                                 00600000
           05  PO-UPVB0032-RC               PIC  9(002).                00610000
               88  UPVB0032-RC-OK                       VALUE 01.       00620000
               88  UPVB0032-RC-NICHT-NUM                VALUE 02.       00630000
               88  UPVB0032-RC-ZUVIEL-VK                VALUE 03.       00640000
               88  UPVB0032-RC-ZUVIEL-NK                VALUE 04.       00650000
               88  UPVB0032-RC-FACHFEHLER-1             VALUE 11.       00660000
                                                                        00670000
           05  PO-UPVB0032-PROZENTSATZ-NUM  PIC S999V99 COMP-3.         00680000
           05  PO-UPVB0032-PROZENTSATZ-DR   PIC  ZZ9,99.                00690000
      *                                                                 00700000
      *                                                                 00710000
131093* 01  PI-UPVB0032.                                                00720000
131093*     05  PI-UPVB0032-MA-RCK-STZ    PIC X(6).                     00730000
131093**                                                                00740000
131093* 01  PO-UPVB0032.                                                00750000
131093*     05  PO-UPVB0032-MA-RCK-STZ    PIC S999V99 COMP-3.           00760000
131093*     05  PO-UPVB0032-DR-RCK-STZ    PIC ZZ9,99.                   00770000
131093*     05  PO-UPVB0032-RC            PIC 9.                        00780000
      *                                                                 00790000
      ******** ENDE COPY-MEMBER (DRVB0032) *******************          00800000
                                                                        00810000
