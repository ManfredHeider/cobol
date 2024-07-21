      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0204.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0204.CPv  $
      *
      *   Rev 2.5   Sep 18 2012 16:15:22   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.4   Mar 11 2004 10:01:56   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.3   Nov 13 2003 16:01:58   t13ex49
      *Vorbereitung post. Prüfung
      *
      *   Rev 2.2   Oct 28 2003 15:53:16   t13ex49
      *UPDATE ADRESSESUCHSTRASSE und ADRESSEGEPRUEFT
      *
      *   Rev 2.1   Oct 13 2003 11:45:58   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   Feb 27 1999 12:53:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   09 Feb 1999 12:34:26   T13EX12
      *AKI00014 - Update Funktion eingebaut
      *
      *   Rev 1.2   29 Oct 1998 17:47:44   T13EX19
      *FKI00010 - Korrektur
      *
      *   Rev 1.1   12 Aug 1998 16:47:12   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:47:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0204  FUNKTIONEN STANDARD-OZP            *
      *                           ADRESSE                            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    23.06.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !16.10.98! 02 ! DELETE                    ! gbe      *
      * -------------------------------------------------------------*
      * AKI00001!05.11.98! 03 ! Neue Update-Funktion      !v.d.Goltz *
070703* AKI00114!05.04.02!    ! Neue Select-Funktion      !Peter Pohl*
      * --------+--------+----+---------------------------+----------*
271003* FKI00789!27.10.03!    ! Update AdresseSuchStrasse !Peter Pohl*
271003*         !        !    ! und AdresseGeprueft       !          *
      *         !        !    ! DRKI1600                  !          *
      * --------+--------+----+---------------------------+----------*
311003* AKI00268!31.10.03!    ! Update                    !Peter Pohl*
311003*         !        !    ! DRKI1608 und DRKI1609     !          *
      * --------+--------+----+---------------------------+----------*
240204* AKI00268!01.03.04!    ! Einführung ...Ortsteil    !Peter Pohl*
240204*         !        !    ! sowie ...PostPruef und    !          *
240204*         !        !    ! PostPruefID               !          *
240204*         !        !    ! Update                    !Peter Pohl*
240204*         !        !    ! DRKI1646 - kompl.         !          *
240204*         !        !    ! DRKI1648 - ohne PLZ,ORT,ST!          *
240204*         !        !    ! Select                    !          *
240204*         !        !    ! DRKI1649 - alle Felder    !          *
      * --------+--------+----+---------------------------+----------*
      * AKI00886!08.06.12!2.5 ! Neue Kundenaktualisierung !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0002.
           05 PI-ZTKI0002-FUNKTION             PIC X(008).
              88 PI-ZTKI0002-DRKI0206-INS          VALUE 'DRKI0206'.
              88 PI-ZTKI0002-DRKI0207-UPD          VALUE 'DRKI0207'.
              88 PI-ZTKI0002-DRKI0205-SEL-ALL      VALUE 'DRKI0205'.
161098        88 PI-ZTKI0002-DRKI0735-DEL          VALUE 'DRKI0735'.
051198        88 PI-ZTKI0002-DRKI0750-UPD-EIN      VALUE 'DRKI0750'.
070703        88 PI-ZTKI0002-DRKI1395              VALUE 'DRKI1395'.
070703        88 PI-ZTKI0002-DRKI1567              VALUE 'DRKI1567'.
271003        88 PI-ZTKI0002-DRKI1600              VALUE 'DRKI1600'.
311003        88 PI-ZTKI0002-DRKI1608              VALUE 'DRKI1608'.
311003        88 PI-ZTKI0002-DRKI1609              VALUE 'DRKI1609'.
240204        88 PI-ZTKI0002-DRKI1646              VALUE 'DRKI1646'.
240204        88 PI-ZTKI0002-DRKI1648              VALUE 'DRKI1648'.
240204        88 PI-ZTKI0002-DRKI1649              VALUE 'DRKI1649'.
080612        88 PI-ZTKI0002-DRKI2620              VALUE 'DRKI2620'.
      *
      ****************************************************************
