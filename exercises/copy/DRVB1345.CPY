      *****************************************************************
      *    COPY-MEMBER: DRVB1345                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0208                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.06.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+-----------*
      * VLA-4865 !02.06.22! NEU                           ! KLEEMEIER *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1345 - ERMITTLUNG KÜNDIGUNGSDATUM FÜR AV-TYP 7*
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1345.
          05 PI-DRVB1345-DATEN.
             10 PI-DRVB1345-MA-ST-NR           PIC S9(009) COMP.
             10 PI-DRVB1345-EINTRITT           PIC  X(010).
             10 PI-DRVB1345-STICHTAG           PIC  X(010).
 
       01 PO-DRVB1345.
          05 PO-DRVB1345-RC                    PIC  X(003).
             88 PO-DRVB1345-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1345-RC-NOK                  VALUE 'NOK'.
          05 PO-DRVB1345-DATEN.
             10 PO-DRVB1345-KUEND-DAT          PIC  X(010).
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1345   --------------------------*
      *---------------------------------------------------------------*