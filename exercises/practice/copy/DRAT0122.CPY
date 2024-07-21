      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0122.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0122.CPv  $
      *
      *   Rev 2.2   Mar 22 2006 15:02:48   t13ae28
      *ASL00216: Übernahme GV ins UU11
      *
      *   Rev 2.1   Mar 09 2006 17:29:36   t13ae28
      *ASL00216: SF in der AV
      *
      *   Rev 2.0   Feb 27 1999 11:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Nov 27 1996 15:20:18   T13AE28
      *FEVB0323 Copyfile im Zuge VSW96 neu angelegt
      *         D.Ehrmann
      *
      *   Rev 1.0   Nov 27 1996 15:13:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0106   KNR-KONST. FUER DARL-STATUS        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                        *
      *    DATUM:    20.05.1996                                       *
      *****************************************************************
      *    VERSION   1    20.05.96 11:56:16
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FEVB0323!21.10.96! 01 ! NEU                       ! D.EHRMANN*
      * ASL00216!15.02.06! 02 ! Sonderfall aufgenommen    ! D.EHRMANN*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE FÜR KTY-VSWZ-BERERCHTIGUNG
      *
       01  FILLER.
           05  K-KNR-G-BEITRAGSFREI           PIC S9(004) COMP VALUE 00.
           05  K-KNR-G-ABSCHLUSSEINH          PIC S9(004) COMP VALUE 01.
           05  K-KNR-G-ZUSATZVEREINB          PIC S9(004) COMP VALUE 03.
150206     05  K-KNR-G-SONDERFALL             PIC S9(004) COMP VALUE 04.
           05  K-KNR-A-BEITRAGSFREI           PIC S9(004) COMP VALUE 00.
           05  K-KNR-A-ABSCHLUSSEINH          PIC S9(004) COMP VALUE 01.
           05  K-KNR-A-ZUSATZVEREINB          PIC S9(004) COMP VALUE 03.
150206     05  K-KNR-A-SONDERFALL             PIC S9(004) COMP VALUE 04.
      *
      ******** ENDE COPY-MEMBER DRAT0122  KNR-K. VSWZ-BERECHTIGUNG ****
