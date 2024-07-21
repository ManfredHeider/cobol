      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0866.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0866.cpv  $
      *
      *   Rev 2.1   Jan 21 2000 14:01:58   T13EX44
      *Datumsangaben erweitert
      *
      *   Rev 2.0   May 26 1999 17:26:40   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:47:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0866                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0866                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Opritz                                           *
      *    DATUM:    18.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * Daisy   !18.02.98!01.01! NEU                      ! Opritz   *
      * AFPA0012!29.07.98!01.02! DATUM-VON und -BIS im    ! Scheidel *
      *         !        !     ! PI-Bereich ergaenzt      !          *
      *         !17.08.98!01.03! SPEICHERORT im PI-Bereich! Scheidel *
      *         !        !     ! ergaenzt                 !          *
      *         !        !     ! PO-DRPA0866-RC-NOK       !          *
      *         !        !     ! ergaenzt                 !          *
      *         !20.10.98!01.04! Unterfunktionen STD und  ! Scheidel *
      *         !        !     ! LAST                     !          *
      *         !        !     ! Parameter DATUM-BIS-ABR  !          *
      *         !        !     !                          !          *
      * APA00073!03.01.00!02.01! ALLE JAHRESANGABEN AUF   ! SCHEIDEL *
      *         !        !     ! VIER STELLEN ERWEITERT   !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FUER DIE FUNKTION DRPA0866: SUCHEN VNR/PIN IN OPAL
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
          01 PI-DRPA0866.
           05 PI-DRPA0866-UFKT         PIC  X(001).
              88 PI-DRPA0866-UFKT-STD              VALUE '1'.
              88 PI-DRPA0866-UFKT-LAST             VALUE '2'.
           05 PI-DRPA0866-DATEN.
            10 PI-DRPA0866-PGEB-GES  PIC S9(004)         BINARY.
            10 PI-DRPA0866-VNR       PIC  9(015).
            10 PI-DRPA0866-PIN       PIC S9(009)         BINARY.
290798*     10 PI-DRPA0866-DATUM-VON PIC  9(006).
030100      10 PI-DRPA0866-DATUM-VON PIC  9(008).
290798*     10 PI-DRPA0866-DATUM-BIS PIC  9(006).
030100      10 PI-DRPA0866-DATUM-BIS PIC  9(008).
201098*     10 PI-DRPA0866-DATUM-BIS-ABR
201098*                              PIC  9(006).
030100      10 PI-DRPA0866-DATUM-BIS-ABR
030100                               PIC  9(008).
170898      10 PI-DRPA0866-SPEICHERORT
170898                               PIC  9(002).
      *
          01 PO-DRPA0866.
           05 PO-DRPA0866-RC         PIC  X(003).
              88 PO-DRPA0866-RC-OK                 VALUE 'OK '.
              88 PO-DRPA0866-RC-NOT-FOUND          VALUE 'NFD'.
170898        88 PO-DRPA0866-RC-NOK                VALUE 'NOK'.
           05 PO-DRPA0866-DATEN.
            10 PO-DRPA0866-PRINTE    PIC S9(15)          PACKED-DECIMAL.
            10 PO-DRPA0866-VERSION   PIC S9(04)          BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
