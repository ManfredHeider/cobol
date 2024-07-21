      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0209.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0209.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 15 1997 13:48:20   T13AE31
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:09:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0209                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0209                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    05.02.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0005!05.02.97!    ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0209
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    JOIN AUF TABELLE PVSG UND PVST
      *                     (TBPA0016 UND TBPA0015)
      ****************************************************************
      *
       01  PI-DRPA0209.
           05  PI-DRPA0209-PROGRAMM            PIC  X(008).
           05  PI-DRPA0209-UFKT-CODE           PIC  X(02).
      *
           05  PI-DRPA0209-AUSWAHL-KRITERIEN.
               10 PI-DRPA0209-PVSG-VSTAT-GRP  PIC S9(004) BINARY.
               10 PI-DRPA0209-VSTAT-MIN       PIC  X(004).
      *
       01  PO-DRPA0209.
           05  PO-DRPA0209-RC                 PIC  X(003).
               88 PO-DRPA0209-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0209-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0209-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0209-DATEN.
               10 PO-DRPA0209-PVST-VSTAT       PIC  X(004).
               10 PO-DRPA0209-KTY-VSTAT-GRP    PIC S9(004) BINARY.
               10 PO-DRPA0209-PVSG-LAST-UPDATE PIC  X(026).
               10 PO-DRPA0209-PVST-TEXT        PIC  X(050).
      *
      ******* ENDE COPY-MEMBER **************************************
