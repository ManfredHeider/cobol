      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:07:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: ÜBERGABEBEREICH  (DRVB0021)                   * 00020000
      *                 ERMITTELN BETREUERNAME, STUFE                 * 00030000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: M. GIESE                                         * 00060000
      *    DATUM:    24.04.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   02   05.10.93                                   *  00090101
      ****************************************************************  00091000
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !24.04.93! 01 ! NEU                       ! GIESE    *  00095002
      *         !30.09.93!    ! AENDERUNG STF-X,TYP-X     ! GIESE    *  00095002
      *         !05.10.93! 02 ! PROD.ÜBERGABE             !          *  00096001
      *         !        !    !                           !          *  00097001
      *         !        !    !                           !          *  00098001
      *         !        !    !                           !          *  00099000
      *         !        !    !                           !          *  00099100
      *         !        !    !                           !          *  00099200
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600

       01   PI-UPVB0029.
         05  PI-UPVB0029-MA-ST-NR         PIC S9(09) COMP.
         05  PI-UPVB0029-ST-TYP           PIC S9(04) COMP.
         05  PI-UPVB0029-MA-MAT-DAT-BIS   PIC  X(10).

       01   PO-UPVB0029.
         05  PO-UPVB0029-RC-MELD-TYP        PIC  X(01).
         05  PO-UPVB0029-RC-MELD-NR         PIC S9(04) COMP.
         05  PO-UPVB0029-BETR-ST-NR         PIC S9(09) COMP.
         05  PO-UPVB0029-BETR-NAME          PIC  X(40).
300993   05  PO-UPVB0029-BETR-STF-X         PIC  X(04).
         05  PO-UPVB0029-BETR-STF           PIC S9(04) COMP.
         05  PO-UPVB0029-BETR-PRV-STZ       PIC  X(03).
         05  PO-UPVB0029-BETR-ST-PRZ        PIC  X(01).
         05  PO-UPVB0029-BETR-ST-TYP        PIC S9(04) COMP.
300993   05  PO-UPVB0029-BETR-ST-TYP-X      PIC  X(04).
         05  PO-UPVB0029-BETR-MA-TYP        PIC S9(04) COMP.
         05  PO-UPVB0029-BETR-MSBZ-DAT-VON  PIC  X(26).
         05  PO-UPVB0029-BETREUUNG-DAT-VON  PIC  X(26).
         05  PO-UPVB0029-TUPEL-BVH          PIC  X(96).
         05  PO-UPVB0029-TUPEL-EDD          PIC  X(88).
      ******** ENDE COPY-MEMBER ÜBERGABEBEREICH        (DRVB0021) ***** 01600000
