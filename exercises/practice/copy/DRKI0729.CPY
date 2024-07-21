      * T13EX47   Stand: 17/11/98    16:23:26
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0729.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0729.CPv  $
      *
      *   Rev 2.3   Mar 27 2017 09:01:36   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.2   Jan 25 2000 15:22:30   T13AE21
      *
      *
      *   Rev 2.1   Mar 08 1999 11:29:14   T13EX49
      *Änderungen für DIT
      *(Source vom Host geholt)
      *
      *   Rev 1.1   08 Feb 1999 18:37:16   T13EX47
      *SELECT in ZTKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:33:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0729                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: gbe                                              *
      *    DATUM:    15.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.10.98! 01 ! NEUERSTELLUNG             !  gbe     *
      * -------------------------------------------------------------*
      * FKI00001!05.03.99! 01 ! Adressid liefern.         !v.d.Goltz *
      * -------------------------------------------------------------*
      * FKI00312!19.01.00!2.2 ! Neue PO-Attribute in      !PHILIPP   *
      *         !        !    ! Funktion DRKI0729         !          *
      *         !        !    ! (VRBGUELTIGKEITSDAT und   !          *
      *         !        !    !  LETZTEAENDERUNG)         !          *
      * -------------------------------------------------------------*
      * FKI01544!22.03.17!    ! III entfernt              !HHU       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0729                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0729.
          05 PI-DRKI0729-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0729-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0729-DATEN.
            07 PI-DRKI0729-vrbvertragsid          PIC X(013).
            07 PI-DRKI0729-vrbpersonenrollekz     PIC X(001).
      *
       01 PO-DRKI0729.
 
          05 PO-DRKI0729-RC                       PIC  X(003).
             88 PO-DRKI0729-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0729-RC-NFD                   VALUE 'NFD'.
 
          05 PO-DRKI0729-DATEN.
            07 PO-DRKI0729-vrbpersonenid          PIC X(013).
            07 PO-DRKI0729-VRBPABADRESSEID        PIC X(013).
            07 PO-DRKI0729-VRBGUELTIGKEITSDAT     PIC X(010).
            07 PO-DRKI0729-LETZTEAENDERUNG        PIC X(026).
      *
      ***** Ende Copy-Member DRKI0729 *****************************

