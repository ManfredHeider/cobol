      *****************************************************************
      *    COPY-MEMBER: DRVB1323                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0205                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    06.12.2021                                       *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * VLA-3482   !06.12.21! NEU                          ! KLEEMEIER*
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1323 - ERMITTELN WERKTAG                      *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1323.
          05 PI-DRVB1323-DATEN.
             10 PI-DRVB1323-STICHTAG           PIC  X(10).
             10 PI-DRVB1323-ANZ-TAGE           PIC S9(03) COMP-3.
 
       01 PO-DRVB1323.
          05 PO-DRVB1323-RC                    PIC  X(03).
             88 PO-DRVB1323-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1323-RC-NOK                  VALUE 'NOK'.
 
          05 PO-DRVB1323-DATEN.
             10 PO-DRVB1323-WERKTAG            PIC  X(10).
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1323   --------------------------*
      *---------------------------------------------------------------*

