      * T13EX47   Stand: 17/11/98    16:23:22
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0484.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0484.CPv  $
      *
      *   Rev 2.1   Feb 22 2000 14:17:26   T13EX39
      *FKI00330 OZP ADRESSID
      *
      *   Rev 1.1   08 Feb 1999 16:53:00   T13EX39
      *FUNKTION UPDATE FUER ZTKI0359
      *
      *   Rev 1.0   28 Jul 1998 09:57:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0484                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0484                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.08.98/18.53.54
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001! 17.8.98!1.0 ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00330!21.02.00!2.3 ! Funktion für DRKI0484     !WHO       *
      *         !        !    ! korrigiert.               !          *
      *         !        !    ! (Replikation hat dafür    !          *
      *         !        !    !  nicht funktioniert)      !          *
      *         !        !    ! Hier: Nicht benötigte     !          *
      *         !        !    ! Attribute entfernt        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0484
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0484.
           05  PI-DRKI0484-KEY.


             10 PI-DRKI0484-VRBPERSONENID   PIC X(13).
             10 PI-DRKI0484-VRBPERSONENROLLEKZ   PIC X(01).
             10 PI-DRKI0484-VRBVERTRAGSID   PIC X(13).


           05  PI-DRKI0484-DATEN.
             10 PI-DRKI0484-VRBPABADRESSEID   PIC X(13).


      *
       01  PO-DRKI0484.
           05  PO-DRKI0484-RC                 PIC  X(003).
               88 PO-DRKI0484-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0484-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
