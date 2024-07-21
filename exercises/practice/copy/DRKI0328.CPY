      * T13EX39   Stand: 21/07/98    21:34:46
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0328.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0328.CPv  $
      *
      *   Rev 2.2   Nov 28 2002 08:40:38   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.1   Jul 30 2002 16:34:08   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Feb 27 1999 12:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:20:56   T13EX39
      *FUNKTIONEN FUER UPKI0043
      *
      *   Rev 1.0   14 Jul 1998 16:59:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *    COPY-MEMBER FÜR UPKI0043                                   *
      *    ---------------------------------------------------------- *
      *                                                               *
      *    DATEITYP  = KSDS                                           *
      *    Länge = 150 bytes                                          *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!02.05.02!01  ! NEUE FKT. DRKI1345        !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00294!18.10.02!    ! neue Funktionen für ISIN  !Peter Pohl*
      *         !        !    ! - Fond-ID anhand ISIN und !          *
      *         !        !    !   Gesellschaft ermitteln  !          *
      *         !        !    ! - Fond-ID anhand ISIN     !          *
      *         !        !    !   ermitteln               !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPKI0043.
           05  PI-UPKI0043-FUNKTION            PIC  X(008).
               88 PI-UPKI0043-GETFONDID             VALUE 'DRKI0329'.
020502         88 PI-UPKI0043-DRKI1435              VALUE 'DRKI1345'.
181002         88 PI-UPKI0043-DRKI1471              VALUE 'DRKI1471'.
181002         88 PI-UPKI0043-DRKI1472              VALUE 'DRKI1472'.
      *
      ******* ENDE COPY-MEMBER **************************************
