      * T13EX33   Stand: 21/09/98    15:35:00
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0304.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DSKI0304.CPv  $
      *
      *   Rev 2.2   Feb 28 2020 11:16:46   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *eilschicht
      *
      *   Rev 2.1   Nov 25 2002 13:58:18   t13ae31
      *AKI319 neuer Beitragszustand 11
      *
      *   Rev 2.0   Mar 23 1999 15:53:26   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   30 Jun 1998 10:24:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0304                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    BEITRAGSZUSTAND - TCKI0304                                *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!24.07.98!01.01! NEUERSTELLUNG            !HUEBNER   *
      * --------!--------!-----!--------------------------!----------*
      * AKI00319!22.11.02!     ! neuer Beitr.zust. 11     !Huebner   *
      * --------!--------!-----!--------------------------!----------*
      * AKI01757!28.02.20! 2.2 ! K-BEITRAG-UNBEKANNT -1   !T13AC20   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-BEITRAGSPFLICHTIG           PIC S9(002) BINARY VALUE 1.
           05 K-BEITRAGSFREI                PIC S9(002) BINARY VALUE 2.
           05 K-GESTUNDET                   PIC S9(002) BINARY VALUE 3.
           05 K-TEILGESTUNDET               PIC S9(002) BINARY VALUE 4.
           05 K-BTGFREI-INVAL               PIC S9(002) BINARY VALUE 5.
           05 K-BTGFREI-ABL-BZ              PIC S9(002) BINARY VALUE 6.
           05 K-BTGFREI-TOD                 PIC S9(002) BINARY VALUE 7.
           05 K-BTGFREI-KUEND               PIC S9(002) BINARY VALUE 8.
           05 K-BTGFREI-EINMALZ             PIC S9(002) BINARY VALUE 9.
           05 K-BTGFREI-SONDERZ             PIC S9(002) BINARY VALUE 10.
021122     05 K-ZULAGENGESPEIST             PIC S9(002) BINARY VALUE 11.
           05 K-BEITRAG-UNBEKANNT           PIC  X(002) VALUE '-1'.
      *
      ********  ENDE COPY-MEMBER DSKI0304  ***********

