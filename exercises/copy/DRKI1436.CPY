      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1436.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1436.CPv  $
      *
      *   Rev 2.1   Oct 06 2003 10:11:50   t13ex49
      *AKI00268 - Dublettenbereinigung
      *erstmalige Übergabe
      *
      *   Rev 2.0   Apr 23 2002 16:03:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1436                                      *
      *    --------------------------------------------------------   *
      *
      *
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!31.01.02!    ! Dubletten ermitteln - NEU !Peter Pohl*
      *         !18.08.03!    ! 1 Byte länger wg.         !          *
      *         !        !    ! Tabellenänderung          !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Dieses Copy enthält den Aufbau der Entladedatei für
      *    die Dublettenbereinigung (Tabelle PERSONADRESSBEZ)
      *    und zusätzlich ein Sortierkriterium
      *
      *    Länge: 56 Byte
      *           57 Byte - 18. 08. 2003
      *
      ****************************************************************

       01  DRKI1436.
           10 DRKI1436-PERSABPERSONENID            PIC X(13).
           10 DRKI1436-PERSABADRESSEID             PIC X(13).
           10 DRKI1436-PERSABADRESSARTID           PIC S9(4) COMP.
           10 DRKI1436-LETZTEAENDERUNG             PIC X(26).
           10 DRKI1436-NIL-LETZTEAENDERUNG         PIC X(01).
           10 DRKI1436-PERSABVERTRADRKZ            PIC X(01).
           10 DRKI1436-SORT                        PIC X(01).

